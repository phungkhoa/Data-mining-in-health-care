#input csv file
load_data<-function(filename)
{
  lines<-read.csv(filename)
  dataset<-c(lines)
  print(dataset)
}

#divide into class
separate_data<-function(dataset)
{
  separated = {}
  for(i in len(dataset))
    {
    vector = dataset[i]
    if (!(vector[-1] %in% separated))
      separated[vector[-1]] = NULL
    separated[vector[-1]] = vector
  }
}

#divide in to TRAINING and TESTING
split_data<-function(dataset, splitRatio)
{
  trainSize = lenght(dataset) * splitRatio
  trainSet<-c()
  copy = list(dataset)
  while (length(trainSet) < trainSize){
    index <- sample(len(copy), 1)
    trainSet<-append(trainSet, copy[[index]])
  }
  
}

#count mean
mean<-function(numbers)
{
  return (sum(numbers) / length(numbers))
}

#count standard deviation
standard_deviation<-function(numbers)
{
  average = mean(numbers)
  variance = 0
  for(x in numbers){
    variance = variance + (x - average)^2
  }
  deviation = sqrt(variance / (length(numbers)-1))
}

#sum
summarize<-function(dataset)
{
  summarises = [(mean(attributes), standard_deviation(attribute)) for(attribute in zip(*dataset))]
  del summarises[-1]
  
  return (summarises)
}
summarize_by_class(dataset)
{
  separated = separate_date(dataset)
  summaries = {}
  for(classValue, instances in separated.item()){
    summaries[classValue] = summarize(instances)
  }
  
  return (summaries)
}

#calculate probability according to Gaussian theorem
calculate_prob<-function(x, mean, stdev)
{
  exponent = exp(-((x - mean)^2) / (2 * stdev^2))
  
  return ((1 / (sqrt(2 * pi) * stdev)) * exponent)
}

#calculate probability for class
calculate_class_prob<-function(summaries, inputVector)
{
  probabilities = {}
  for(classValue, classSummaries in summaries.items()){
    probabilities[classValue] = 1
    for(i in length(classSummaries)){
      mean, stdev = classSummaries[i]
      x = inputVector[i]
      probabilities[classValue] = probabilities[classValue] * calculate_prob(x, mean, stdev)
    }
  }
  
  return (probabilities)
}

#predict which class Vector belongs
predict<-function(summaries, inputVector)
{
  probabilities = calculate_class_prob(summaries, inputVector)
  bestLabel, bestProb = None, -1
  for(classValue, probability in probabilities.items()){
    if (bestLabel is None or probability > bestProb){
      bestProb = probability
      bestLabel = classValue
    }
  }
  
  return (bestLabel)
}

#predict which class TESTING belongs
get_predictions<-function(summaries, testSet)
{
  predictions = []
  for(i in length(testSet)){
    result = predict(summaries, testSet[i])
    predictions<-append(predictions, result)
  }
  
  return (predictions)
}

#cal accuracy of class
get_accuracy<-function(testSet, predictions)
{
  correct = 0
  for(i in length(testSet)){
    if (testSet[[i]][[-1]] == predictions[[i]]){
      correct = correct + 1
    }
  }
  
  return (correct / length(testSet) * 100.0)
}
get_data_label<-function(dataset)
{
  data = []
  label = []
  for(x in dataset){
    data<-append(data, x[:8])
    label<-append(label, x[-1])
  }
  
  return (data, label)
}

#test
#list_data <- c(5, 4, 3, 2, 1)
#print(standard_deviation(list_data))
#load_data("tieu_duong.csv")