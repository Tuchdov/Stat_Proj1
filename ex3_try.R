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

# maybe we should add "pet name" attribute or a data frame??

names(time_dog) <-  rep("dog", length(time_dog))
names(time_cat) <-  rep("cat", length(time_cat))

time_costumer  = sort(c(time_dog, time_cat))
n = length(time_costumer)


# sanity check
names(time_costumer)[1] == "dog"
names(time_costumer) == "dog"

tipul_duration <- function(pet_vec, index){
  name = names(pet_vec)[index]
  if ( name == "dog"){
    serv_time = rexp(1, rate = 3)
  }
  else{
    serv_time = rexp(1, rate = 5)
  }
  return(serv_time)
}


interval <-  function(arrival, tipul){
  int = numeric(n)
  int[1] = 0
  int[2] = time_costumer[1]
  tor = 0 
  i = 3
  
  sof_tipul = numeric(n)
  sof_tipul[1] = tipul_duration(arrival, index = 1)
  
  tipul_num = 1
  arrival_num = 1
  # first_costumer_in_line = cat or dog names of next arrival when tor == 0' 
  # only saved when arrival_time > sof_tipul
  while(int[i] < 720){
    if (tor == 0){
      if (sof_tipul[tipul_num] < arrival[arrival_num]){
        # the tor didn't change start next tipul
        tipul_num = tipul_num + 1
        # The next sof_tipul is the time the pet arrived and it's tipul duration
        sof_tipul[tipul_num] = arrival[arrival_num] + tipul_duration(arrival, index = arrival_num) 
      }
      else{
        tor = tor + 1
        int[i] = arrival[arrival_num]
        i = i + 1
        arrival_num = arrival_num + 1
        # heres the change, defining who is the 1st customer cat or dog
      }
    }
    # case when line num betwwen 1-9
    else if (tor > 0 && tor < 10){
      if (sof_tipul[tipul_num] < arrival[arrival_num]){
        # the tor yored 1 ,start next tipul
        tipul_num = tipul_num + 1
        if(tor == 1 && fst_costumer )
        tor = tor - 1
        int[i] = sof_tipul[tipul_num]
        i = i +1
      }
      else if (sof_tipul[tipul_num] > arrival[arrival_num]){
        if (names(arrival[arrival_num]) == "dog"){
        tor = tor + 1
        arrival_num = arrival_num + 1
        int[i] = arrival[arrival_num]
        i = i +1 
        }
      }
      
    }
  }
}

# building intervals 1
# intervals are just changes in line/service
int = numeric(n)
# first int starts at time 0
int[0] = 0
# 1st interval ends with the first arrival
int[1] = time_costumer[1]
# the first customer is either cat or a dog
tipul[1] = min(c(time_cat[1],time_dog[1]))
# cat customer
if (time_cat[1] <= time_dog[1]){                   
  tipul[2] =  tipul[1] + rexp(1, serv_cat)
}
# dog customer
else{
  tipul[2] = tipul[1] + rexp(1, serv_dog)
}
# the next change in line is the earlier of end of 2nd tipul
# or the arrival of next customer
int[2] = min(tipul[2], time_costumer[3])

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

if (tor > 0 && tor < 10){
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


# building intervals 2
# intervals are just changes in line/service
int = numeric(n)
# first int starts at time 0
int[0] = 0
# 1st interval ends with the first arrival
int[1] = min(c(time_cat[1],time_dog[1]))
# the first customer is either cat or a dog
tipul[1] = min(c(time_cat[1],time_dog[1]))
                # cat customer
               if (time_cat[1] <= time_dog[1]){                   
                 tipul[2] =  tipul[1] + rexp(1, serv_cat)
               }
                # dog customer
               else{
                 tipul[2] = tipul[1] + rexp(1, serv_dog)
               }
# the next change in line is the earlier of end of 2nd tipul
# or the arrival of next customer
int[2] = min(tipul[2], time_costumer[3])

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

if (tor > 0 && tor < 10){
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





serv_pet <-  function(x){
  serv_dog = 3
  serv_cat = 5
  serv_time = numeric(n)
  tor = 0
  i = 1
  j = 1
  serv_time[1] = 0
  serv_time[2] = min(c(time_cat[i],time_dog[j]))
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


אם אין תור ועוד לא הגיע לקוח
אז הלוקח הבא שיגיע יכנס
סוף הטיפול הבא יהיה זמן הגעה + סוג טיפול לקוח הבא
