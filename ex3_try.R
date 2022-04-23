# arrival time of pet
time_pet = function(rate) {
  vec = numeric(1000)
  i = 1
  vec[1] = rexp(1, rate = rate)
  # build the vector of arrival time
  while  (vec[i] < 720) {
      i = i+ 1
      vec[i] = rexp(1, rate = rate) + vec[i-1]
  }
  # return only values less then 720
  return(vec[-i])
}

time_dog = time_pet(3)
time_cat <- time_pet(1.5)
try1 = sort(c(time_dog, time_cat))
n = length(time_cat) + length(time_dog)
# creating the intervals

serv_pet <-  function(x){
  serv_dog = 3
  serv_cat = 5
  serv_time = numeric(n)
  tor = 0
  i = 1
  j = 1
  serv_time[1] = 0
  serv_time[2] = min(c(time_cat[i],time_dog[j])
  # choosing who te serve
  if (time_cat[1] <= time_dog[1]){                   
  serv_time[3] =  serv_time[2] + rexp(1, serv_cat)
  }
  else{
    serv_time[3] = serv_time[2] + rexp(1, serv_dog)
  }
  if (tor > 1 and tor < 9 ){
    serv_time[next] = serv_time[last] + rexp(1, serv_dog)
    
  }
}




# bulding intervals
int = numuric(n)
int[0] = 0
int[1] = min(c(time_cat[1],time_dog[1])
tipul[1] = min(c(time_cat[1],time_dog[1])
               if (time_cat[1] <= time_dog[1]){                   
                 tipul[2] =  tipul[1] + rexp(1, serv_cat)
               }
               else{
                 tipul[2] = tipul[1] + rexp(1, serv_dog)
               }
int[2] = min(tipul[2], try1[3])

if (tor == 0){
  if (time_cat[1] <= time_dog[1]){
    tipul[next] = tipul[previous] + rexp(1, serv_cat)
    int[next] =  tipul[next]
  }
  else {
    tipul[next] = tipul[previous] + rexp(1, serv_dog)
    int[next] =  tipul[next]
  }
}

if (tor > 0 and tor < 10){
  if (tipul[i] < time_dog[j]){
    int[next] = tipul[i]
    tor = tor - 1
    i = i + 1
  }
  else if (tipul[i] > time_dog[j]){
    tor = tor + 1
    j = j +1
  }
  
}

if (tor == 10){
  int[next] = tipul[next + 1]
  tor = tor -1 
}