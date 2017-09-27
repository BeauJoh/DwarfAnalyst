
source("utils.R")
source("stats.R")
source("aes.R")
source("functions.R")

devices <- c('xeon_es-2697v2','i7-6700k','titanx','gtx1080','gtx1080ti','k40c','knl')#,'firepro_s9150')
sizes <- c('tiny','small','medium','large')
#load data if it doesn't exist in this environment
#to force a reload:
#remove(data.kmeans)

SumPerRunReduction <- function(x){
    z <- data.frame()
    for (y in unique(x$run)){
        z <- rbind(z,data.frame('time'=sum(x[x$run == y,]$time),'run'=y))
    }
    return(z)
}

if (!exists("data.kmeans")){
    data.kmeans <- data.frame()
    columns <- c('region','number_of_objects','number_of_features','iteration_number_hint_until_convergence','id','time','overhead')
    for(device in devices){
        for(size in sizes){
            path = paste("./time_data/",device,"_kmeans_",size,"_time.0/",sep='')
            print(paste("loading:",path))
            x <- ReadAllFilesInDir.AggregateWithRunIndex(dir.path=path,col=columns)
            x$device <- device
            x$size <- size
            data.kmeans <- rbind(data.kmeans, x)
        }
    }
}

if(!exists("data.lud")){
    data.lud <- data.frame(data.frame())
    columns <- c('region','matrix_dimension','id','time','overhead')
    for(device in devices){
        for(size in sizes){
            path = paste("./time_data/",device,"_lud_",size,"_time.0/",sep='')
            print(paste("loading:",path))
            x <- ReadAllFilesInDir.AggregateWithRunIndex(dir.path=path,col=columns)
            x$device <- device
            x$size <- size
            data.lud <- rbind(data.lud, x)
        }
    }
}

if(!exists("data.csr")){
    data.csr <- data.frame()
    columns <- c('region','number_of_matrices','workgroup_size','execution_number','id','time','overhead')
    for(device in devices){
        for(size in sizes){
            path = paste("./time_data/",device,"_csr_",size,"_time.0/",sep='')
            print(paste("loading:",path))
            #SeparateAllNAsInFilesInDir(dir.path=path) 
            x <- ReadAllFilesInDir.AggregateWithRunIndex(dir.path=path,col=columns)
            x$device <- device
            x$size <- size
            data.csr <- rbind(data.csr, x)
        }
    }
}

if(!exists("data.fft")){
    data.fft <- data.frame()
    columns <- c('signal_length','region','id','time','overhead')
    for(device in devices){
        for(size in sizes){
            path = paste("./time_data/",device,"_openclfft_",size,"_time.0/",sep='')
            print(paste("loading:",path))
            x <- ReadAllFilesInDir.AggregateWithRunIndex(dir.path=path,col=columns)
            x$device <- device
            x$size <- size
            data.fft <- rbind(data.fft, x)
        }
    }
}

if(!exists("data.dwt")){
    data.dwt <- data.frame()
    columns <- c('region','dwt_level','id','time','overhead')
    for(device in devices){
        for(size in sizes){
            path = paste("./time_data/",device,"_dwt2d_",size,"_time.0/",sep='')
            print(paste("loading:",path))
            x <- ReadAllFilesInDir.AggregateWithRunIndex(dir.path=path,col=columns)
            x$device <- device
            x$size <- size
            data.dwt <- rbind(data.dwt, x)
        }
    }
}

if(!exists("data.gem")){
    data.gem <- data.frame()
    columns <- c('number_of_residues','number_of_vertices','region','id','time','overhead')
    for(device in devices){
        for(size in sizes){
            path = paste("./time_data/",device,"_gem_",size,"_time.0/",sep='')
            print(paste("loading:",path))
            x <- ReadAllFilesInDir.AggregateWithRunIndex(dir.path=path,col=columns)
            x$device <- device
            x$size <- size
            data.gem <- rbind(data.gem, x)
        }
    }
}

if(!exists("data.srad")){
    data.srad <- data.frame()
    columns <- c('region','id','time','overhead')
    for(device in devices){
        for(size in sizes){
            path = paste("./time_data/",device,"_srad_",size,"_time.0/",sep='')
            print(paste("loading:",path))
            x <- ReadAllFilesInDir.AggregateWithRunIndex(dir.path=path,col=columns)
            x$device <- device
            x$size <- size
            data.srad <- rbind(data.srad, x)
        }
    }
}

if (!exists("data.all")){
    data.all <- data.frame()
    print("munging data...")
    ##parse kmeans
    for(device in devices){
        for(size in sizes){
            x <- data.kmeans[data.kmeans$region=="kmeans_kernel" & data.kmeans$device == device & data.kmeans$size == size,]
            x <- SumPerRunReduction(x)
            browser()
            data.all <- rbind(data.all,data.frame('application'='kmeans',
                                                  'device'=device,
                                                  'size'=size,
                                                  'time'=x$time,
                                                  'run'=x$run))
        }
    }
    #munge lud
    for(device in devices){
        for(size in sizes){
            x <- data.lud[(data.lud$region=="diagonal_kernel"|data.lud$region=="perimeter_kernel"|data.lud$region=="internal_kernel") & data.lud$device == device & data.lud$size == size,]
            x <- SumPerRunReduction(x)
            data.all <- rbind(data.all,data.frame('application'='lud',
                                                  'device'=device,
                                                  'size'=size,
                                                  'time'=x$time,
                                                  'run'=x$run))
        }
    }
    #munge csr
    for(device in devices){
        for(size in sizes){
            x <- data.csr[data.csr$region=="csr_kernel" & data.csr$device == device & data.csr$size == size,]
            x <- SumPerRunReduction(x)
            data.all <- rbind(data.all,data.frame('application'='csr',
                                                  'device'=device,
                                                  'size'=size,
                                                  'time'=x$time,
                                                  'run'=x$run))
        }
    }
    #munge fft
    for(device in unique(data.fft$device)){#devices){
        for(size in sizes){
            x <- data.fft[data.fft$region=="fft_kernel" & data.fft$device == device & data.fft$size == size,]
            x <- SumPerRunReduction(x)
            data.all <- rbind(data.all,data.frame('application'='fft',
                                                  'device'=device,
                                                  'size'=size,
                                                  'time'=x$time,
                                                  'run'=x$run))
        }
    }
    #munge dwt
    for(device in unique(data.dwt$device)){#devices){
        for(size in sizes){
            x <- data.dwt[data.dwt$region=="kl_fdwt53Kernel_kernel" & data.dwt$device == device & data.dwt$size == size,]
            x <- SumPerRunReduction(x)
            data.all <- rbind(data.all,data.frame('application'='dwt',
                                                  'device'=device,
                                                  'size'=size,
                                                  'time'=x$time,
                                                  'run'=x$run))
        }
    }
    #munge gem
    for(device in unique(data.gem$device)){#devices){
        for(size in sizes){
            x <- data.gem[data.gem$region=="gem_kernel" & data.gem$device == device & data.gem$size == size,]
            #partial_sum_stride <- length(unique(na.omit(x$id)))
            #times <- colSums(matrix(x$time, nrow=partial_sum_stride))
            x <- SumPerRunReduction(x)
            data.all <- rbind(data.all,data.frame('application'='gem',
                                                  'device'=device,
                                                  'size'=size,
                                                  'time'=x$time,
                                                  'run'=x$run))
        }
    }
    #munge srad
    for(device in unique(data.srad$device)){#devices){
        for(size in sizes){
            x <- data.srad[(data.srad$region=="srad1_kernel" | data.srad$region=="srad2_kernel") & data.srad$device == device & data.srad$size == size,]
            #partial_sum_stride <- length(unique(na.omit(x$id)))
            #times <- colSums(matrix(x$time, nrow=partial_sum_stride))
            x <- SumPerRunReduction(x)
            data.all <- rbind(data.all,data.frame('application'='srad',
                                                  'device'=device,
                                                  'size'=size,
                                                  'time'=x$time,
                                                  'run'=x$run))
        }
    }
}



