##########################
# Tutorial : Reading Audio File
# Author : Taposh Roy
# 
##########################

#########################
#### Using TuneR Package ######
#########################
#Read a Wave File
library(readr)
library(tuneR)
#path of file
file_audio_path <- '../audio_file.wav'
#Read Files
train_audio = readWave(file_audio_path)

#Lets see the structure of the audio.
str(train_audio)

#Observe:
#the wav file has one channel (@left) containing 18593 sample points each, considering the sample rate (sndObj@samp.rate = 4000) 
#this corresponds to a duration of 4.6s:
#  396900/train_audio@samp.rate = 4.6sec

s1 <- train_audio@left
#the readWave function reads wav files as integer types. 
#Our wav file has a 16-bit depth (train_audio@bit), this means 
#that the sound pressure values are mapped to integer values 
#that can range from -2^15 to (2^15)-1. We can convert our 
#sound array to floating point values ranging from -1 to 1 as follows:

s1 <- s1 / 2^(train_audio@bit -1)

#Plotting the Tone
#A time representation of the sound can be obtained by plotting 
#the pressure values against the time axis. However we need to create
#an array containing the time points first:
timeArray <- (0:(18593-1)) / train_audio@samp.rate
#Plot the wave
plot(timeArray, s1, type='l', col='black', xlab='Time (ms)', ylab='Amplitude') 

#Advanced tuneR
m2 <- melfcc(train_audio, numcep=9, usecmp=TRUE, modelorder=8, 
             spec_out=TRUE, frames_in_rows=FALSE)
str(m2)


#########################
#### Using Wrassp Package ######
#########################

#wrassp is capable of more than just the mere reading and writing of specific signal file formats. 
#We will now use wrassp to calculate the formant values, 
#their corresponding bandwidths, the fundamental frequency contour and the RMS energy contour 
#of the audio file.

library(wrassp)
# create path to wav file
#file_audio_path <- '/Users/p624626/Documents/A20/datascience/ecg/HB_data/train/HB_1.wav'
# read audio file
au = read.AsspDataObj(file_audio_path)
str(au)

# (only plot every 10th element to accelerate plotting)
plot(seq(0,numRecs.AsspDataObj(au) - 1, 10) / rate.AsspDataObj(au), 
     au$audio[c(TRUE, rep(FALSE,9))], 
     type='l', 
     xlab='time (s)', 
     ylab='Audio samples')

# calculate formants and corresponding bandwidth values
fmBwVals = forest(file_audio_path, toFile=F)

# plot the first 100 F1 values over time:
plot(fmBwVals$fm[1:100,1],type='l')

# due to toFile=F this returns an object of the type AsspDataObj and 
# prevents the result being saved to disc as an SSFF file
class(fmBwVals)

# extract track names
# this time the object contains muliple tracks (formants + their bandwidths)
tracks.AsspDataObj(fmBwVals)

# with more than one field (in this case 250 F1/F2/F3/F4 values)
dim(fmBwVals$fm)

# plot the first 100 F1 values over time:
plot(fmBwVals$fm[1:100,1],type='l')

# plot the formant values
matplot(seq(0,numRecs.AsspDataObj(fmBwVals) - 1) / rate.AsspDataObj(fmBwVals) + 
          attr(fmBwVals, 'startTime'), 
        fmBwVals$fm, 
        type='l', 
        xlab='time (s)', 
        ylab='Formant frequency (Hz)')

#Plot the bandwith
plot(fmBwVals$bw)
#Plot the formant
plot(fmBwVals$fm)

extract_audio_features <- function(x) {
  #tuneR
  tr <- readWave(x) # load file
  #print(t@left)
  ar <- read.AsspDataObj(x)
  
  #File Name
  fname <- file_path_sans_ext(basename(x))
  #add Feature Number of Samples
  num_samples <- numRecs.AsspDataObj(ar)
  # calculate formants and corresponding bandwidth values
  fmBwVals <- forest(x,toFile=F)
  fmVals <- fmBwVals$fm
  bwVals <- fmBwVals$bw
  #add Feature Sample Rate
  sample_rate <- tr@samp.rate
  left= tr@left
  #left
  range_audio = range(tr@left)
  #add Feature min_amplitude_range
  min_range =range_audio[1]
  #add Feature min_amplitude_range
  max_range =range_audio[2]
  normvalues=left/2^(tr@bit -1) 
  normal_range <- range(normvalues)
  #add Feature normalized_min_amplitude_range
  normal_min_ampl_range <- normal_range[1]
  #add Feature normalized_min_amplitude_range
  normal_max_ampl_range <- normal_range[2]
  mylist <- c(fname=fname,num_samples=num_samples,sample_rate=sample_rate, min_range=min_range, max_range=max_range, normal_min_ampl_range=normal_min_ampl_range, normal_max_ampl_range=normal_max_ampl_range,fmVals=fmVals,bwVals=bwVals)
  return(as.data.frame(mylist))
}

file_audio_path <- '../audio_file.wav'
output = extract_audio_features(file_audio_path)
head(output,10)
