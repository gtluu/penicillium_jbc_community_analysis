library(xcms)
library(MSnbase)
library(gtools)

#register(SerialParam(progressbar=TRUE, stop.on.error=FALSE))
#register(SnowParam(workers=16, progressbar=TRUE, stop.on.error=FALSE))

setwd('data/2022MAR15_jbc_135e_lcms/calibrated/ms1_mzml')

samples <- c('blank', 'cca', '135e', 'jbc', 'jbc_135e')
samples <- combinations(n=length(samples), r=2, v=samples)

files <- list.files(pattern='\\.mzML', recursive=FALSE, full.names=TRUE)

setwd('data/2022MAR15_jbc_135e_lcms/r')

get_filenames <- function(files, sample, biorep='all', techrep='all') {
  tmp <- c()
  
  for (filename in files) {
    name <- strsplit(filename, '\\./')[[1]][2]
    name <- strsplit(name, '\\.')[[1]][1]
    name <- strsplit(name, '_')[[1]]
    if (length(name)==6) {
      name <- list('sample'=name[1], 'biorep'=name[2], 'techrep'=name[3])
    } else if (length(name)==7) {
      name <- list('sample'=paste(name[1], name[2], sep='_'), 'biorep'=name[3], 'techrep'=name[4])
    }
    
    if (biorep != 'all' & techrep != 'all') {
      if (name['sample']==as.character(sample) &
          name['biorep']==as.character(biorep) &
          name['techrep']==as.character(techrep)) {
        tmp <- c(tmp, filename)
      }
    } else if (biorep != 'all') {
      if (name['sample']==as.character(sample) &
          name['biorep']==as.character(biorep)) {
        tmp <- c(tmp, filename)
      }
    } else if (techrep != 'all') {
      if (name['sample']==as.character(sample) &
          name['techrep']==as.character(techrep)) {
        tmp <- c(tmp, filename)
      }
    } else {
      if (name['sample']==as.character(sample)) {
        tmp <- c(tmp, filename)
      }
    }
  }
  
  return(tmp)
}

get_sample_types <- function(filenames) {
  sample_type <- c()
  for (filename in filenames) {
    name <- strsplit(filename, '\\./')[[1]][2]
    name <- strsplit(name, '\\.')[[1]][1]
    name <- strsplit(name, '_')[[1]]
    if (length(name)==6) {
      sample_type <- c(sample_type, name[1])
    } else if (length(name)==7) {
      sample_type <- c(sample_type, paste(name[1], name[2], sep='_'))
    }
  }
  return(sample_type)
}

for (i in 1:nrow(samples)) {
  sample <- samples[i,]
  sample_files_1 <- get_filenames(files, sample[1])
  sample_files_2 <- get_filenames(files, sample[2])
  sample_files <- c(sample_files_1, sample_files_2)
  
  setwd('data/2022MAR15_jbc_135e_lcms/calibrated/ms1_mzml')
  
  xset <- try(xcmsSet(sample_files,
                      method='centWave',
                      ppm=5,
                      peakwidth=c(5, 20),
                      snthresh=6,
                      prefilter=c(3, 100),
                      noise=100,
                      mzdiff=0.01,
                      integrate=1))
  if (class(xset) == 'try-error') {next}
  xsetg <- try(group(xset, bw=5))
  if (class(xsetg) == 'try-error') {next}
  xsetretcor <- try(retcor(xsetg, method='obiwarp', profStep=1))
  if (class(xsetretcor) == 'try-error') {
    xsetretcor <- try(retcor(xsetg, method='obiwarp', profStep=0.9))
    if (class(xsetretcor) == 'try-error') {next}
  }
  xsetg2 <- try(group(xsetretcor, bw=5))
  if (class(xsetg2) == 'try-error') {next}
  xsetfill <- try(fillPeaks(xsetg2))
  if (class(xsetfill) == 'try-error') {next}
  
  xsetfill@phenoData$class <- as.factor(c(rep(sample[1], length(sample_files_1)),
                                          rep(sample[2], length(sample_files_2))))
  report <- diffreport(xsetfill, sample[1], sample[2])
  
  setwd('data/2022MAR15_jbc_135e_lcms/r')
  save.image(paste0(sample[1], '_vs_', sample[2], '.RData'))
}
