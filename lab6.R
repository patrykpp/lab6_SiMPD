library("neuralnet")

#Going to create a neural network to perform prediction
#Type ?neuralnet for more information on the neuralnet library

#Generate training data
#And store them as a dataframe
traininginput <- as.data.frame(matrix(c(7.9 , 32 , 2 ,
                                        7.9 , 32 , 2 ,
                                        7.9 , 128 , 2,
                                        9.7 , 32 , 2 ,
                                        7.9 , 128 , 2 ,
                                        12.9 , 128 , 4 ,
                                        12.9 , 128 , 4 ,
                                        9.7 , 256 , 2 ,
                                        12.9 , 256 , 4 ,
                                        7.9 , 16 , 2 ), nrow=10, ncol=3))
trainingoutput <- c(2499, 2399, 2899, 3599, 2999, 4949, 5199, 4599, 5699, 1869)

#Column bind the data into one variable
trainingdata <- cbind(traininginput, trainingoutput)

# Create Vector of Column Max and Min Values
maxs <- apply(trainingdata[,], 2, max)
mins <- apply(trainingdata[,], 2, min)

# Use scale() and convert the resulting matrix to a data frame
scaled.trainingdata <- as.data.frame(scale(trainingdata[,], center=mins, scale=maxs-mins))
trainingdata <- scaled.trainingdata

# Check out results
print(head(trainingdata, 10))

colnames(trainingdata) <- c("Wyswietlacz", "Pojemnosc", "Pamięć RAM", "Cena") 
print(trainingdata)

#Train the neural network
#Going to have C(6, 5, 3) hidden layers
#Threshold is a numeric value specifying the threshold for the partial
#derivatives of the error function as stopping criteria.
net.price <- neuralnet(price~Wyswietlacz+pojemnosc+RAM, trainingdata, hidden=c(6, 5, 3), threshold=0.001)
print(net.price)

#Plot the neural network
plot(net.price)

#Test the neural network on some training data
testdata <- as.data.frame(matrix(c(7.9, 32, 2,
                                   9.7, 256, 4,
                                   12.9, 128, 2), nrow=3, ncol=3))
scaled.testdata <- as.data.frame(scale(testdata[,], center=mins[1:3], scale=maxs[1:3]-mins[1:3]))
net.results <- compute(net.price, scaled.testdata) #Run them through the neural network

#Lets see what properties net.price has
ls(net.results)

#Lets see the results
print(net.results$net.result)
