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

tipul_duration_pet <- function(name){
  kind = name
  if ( kind == "dog"){
    serv_time = rexp(1, rate = 3)
  }
  else{
    serv_time = rexp(1, rate = 5)
  }
  return(serv_time)
}

tipul_peyment_pet <-  function(name){
  
  kind = name
  if ( kind == "dog"){
    return(1)
  }
  else{
    return(3)
  }

}

avg_tor_hat <- function(interval_time, tor_size){
  result = (sum(interval_time*tor_size))/720
  return(result)
}

#function starts here





interval <-  function(){
  
  time_dog = time_pet(3)
  time_cat <- time_pet(1.5)
  
  # adding "pet name" attribute or a data frame??
  
  names(time_dog) <-  rep("dog", length(time_dog))
  names(time_cat) <-  rep("cat", length(time_cat))
  
  # time of arrival of all costumers
  arrival  = sort(c(time_dog, time_cat))
  n = length(arrival)
  
  
  int = numeric(5*n)
  int[1] = 0
  int[2] = arrival[1]
  i = 3
  
  tor = numeric(5*n)
  tor[1] = 0
 
  
  sof_tipul = numeric(5*n)
  sof_tipul[1] = arrival[1] + tipul_duration(arrival, index = 1)
  
  # payment of bals
  payment = numeric(5*n)
  payment[1] = tipul_peyment_pet(names(arrival[1]))
  # counter of dog rejection
  dog_reject_count = 0
  
  
  tipul_num = 1
  arrival_num = 2
  tor_indx = 1
  # first_costumer_in_line = cat or dog names of next arrival when tor == 0' 
  # only saved when arrival_time > sof_tipul
  while(int[i] < 720 & arrival_num < n - 1 ){
    # fail safe
    if (tor[tor_indx] < 0){
      tor[tor_indx] = 0
    }
    # case when tor = 0
    else if (tor[tor_indx] == 0){
      # reset the 1st in tor
      rishon_intor = c()
     if (sof_tipul[tipul_num] <= arrival[arrival_num]){
        # the tor didn't change start next tipul
        tipul_num = tipul_num + 1
        # The next sof_tipul is the time the pet arrived and it's tipul duration
        int[i] = arrival[arrival_num]
        sof_tipul[tipul_num] = arrival[arrival_num] + tipul_duration(arrival, index = arrival_num)
        arrival_num = arrival_num+ 1
        i = i + 1
        #payment[tipul_num] = tipul_peyment_pet(names(arrival[arrival_num]))
      }
      else{
        tor_indx = tor_indx + 1
        tor[tor_indx] = tor[tor_indx - 1] + 1
        int[i] = arrival[arrival_num]
        # heres the change, defining who is the 1st customer cat or dog
        rishon_intor = names(arrival[arrival_num])
        i = i + 1
        arrival_num = arrival_num + 1
      }
    }
    
    # case when line num betwwen 1-9
    else if (tor[tor_indx] > 0 && tor[tor_indx] < 10){
      if (sof_tipul[tipul_num] < arrival[arrival_num]){
        # the tor yored 1 ,start next tipul
        tor_indx = tor_indx + 1
        payment[tipul_num] = tipul_peyment_pet(rishon_intor)
        tor[tor_indx] = tor[tor_indx - 1] - 1
        tipul_num = tipul_num + 1
        int[i] = sof_tipul[tipul_num]
        # מה הזמן של הטיפול הבא?
       # next tipul is imiddently after and the time it takes to service the rishon intor
        sof_tipul[tipul_num] = sof_tipul[tipul_num - 1] + tipul_duration_pet(rishon_intor)
        rishon_intor = "dog"
        i = i +1
      }
      
      else{
        if (names(arrival[arrival_num]) == "dog"){
        tor_indx = tor_indx + 1
        tor[tor_indx] = tor[tor_indx - 1] + 1
        arrival_num = arrival_num + 1
        int[i] = arrival[arrival_num]
        i = i +1 
        }
        # a cat has arrived' only update to the next costumer
        else{
          arrival_num = arrival_num + 1
        }
      }
      
    }
    
    #case when tor = 10 (is full)
    else{
      if (sof_tipul[tipul_num] <= arrival[arrival_num]){
      tor_indx = tor_indx + 1
      
      tor[tor_indx] = tor[tor_indx - 1] - 1
      int[i] = sof_tipul[tipul_num]
      payment[tipul_num] = tipul_peyment_pet(rishon_intor)
      tipul_num = tipul_num + 1
      # מה הזמן של הטיפול הבא?
      # next tipul is imiddently after and the time it takes to service the rishon intor
      sof_tipul[tipul_num] = sof_tipul[tipul_num - 1] + tipul_duration_pet(rishon_intor)
      rishon_intor = "dog"
      i = i +1
      }
      # a customer arrived and tor is full
      else{
        if(names(arrival[arrival_num]) == "dog"){
          dog_reject_count = dog_reject_count + 1
          arrival_num = arrival_num + 1
        }
        else{
          arrival_num = arrival_num + 1
        }
        
      }
    }
  }
  
  time_int = numeric(i - 1)
  for (j in (2:i)){
    time_int[j-1] = int[j] - int[j-1]
  }
  
  avg_estimate = avg_tor_hat(time_int, tor[1:(i-1 )])
  
  # return the avg tor 
  profit = sum(payment) - 0.1*dog_reject_count
  dog_num = sum(payment == 1)
  cat_num = sum(payment == 3)
  answer_list = list("profit" = profit, "dog_num" = dog_num,"dog_reject_count" = dog_reject_count,"cat_num" =  cat_num,
                    "est_avg_tor" = avg_estimate )

  #return(c(length(tor[1:(tor_indx+1)]), length(time_int),tor_indx))
  return(answer_list)
  
}


interval()
ans = interval(arrival = time_costumer)

sim_3a_profit = mean(replicate(100,interval()$profit))
sim_3a_avg = mean(replicate(100,interval()$est_avg_tor))
sim_3a_dog_num = mean(replicate(100,interval()$dog_num))
sim_3a_cat_num = mean(replicate(100,interval()$cat_num))
sim_3a_reject_count = mean(replicate(100,interval()$dog_reject_count))

paste("The expected value of profit:" , round(sim_3a_profit,3)) 
paste("The expected value of avg tor:" , round(sim_3a_avg,3)) 
paste("The expected value of dogs served:" , round(sim_3a_dog_num ,3)) 
paste("The expected value of cats served:" , round(sim_3a_cat_num,3)) 
paste("The expected value of cat's rejected:" , round(sim_3a_reject_count,3)) 


interval_2a <-  function(){
  
  time_dog = time_pet(3)
  time_cat <- time_pet(1.5)
  
  # adding "pet name" attribute or a data frame??
  
  names(time_dog) <-  rep("dog", length(time_dog))
  names(time_cat) <-  rep("cat", length(time_cat))
  
  # time of arrival of all costumers
  arrival  = sort(c(time_dog, time_cat))
  n = length(arrival)
  
  
  int = numeric(5*n)
  int[1] = 0
  int[2] = arrival[1]
  i = 3
  
  tor = numeric(5*n)
  tor[1] = 0
  
  
  sof_tipul = numeric(5*n)
  sof_tipul[1] = arrival[1] + tipul_duration(arrival, index = 1)
  
  # payment of bals
  payment = numeric(5*n)
  payment[1] = tipul_peyment_pet(names(arrival[1]))
  # counter of dog rejection
  dog_reject_count = 0
  
  
  tipul_num = 1
  arrival_num = 2
  tor_indx = 1
  # first_costumer_in_line = cat or dog names of next arrival when tor == 0' 
  # only saved when arrival_time > sof_tipul
  while(int[i] < 720 & arrival_num < n - 1 ){
    # fail safe
    if (tor[tor_indx] < 0){
      tor[tor_indx] = 0
    }
    # case when tor = 0
    else if (tor[tor_indx] == 0){
      # reset the 1st in tor
      rishon_intor = c()
      if (sof_tipul[tipul_num] <= arrival[arrival_num]){
        # the tor didn't change start next tipul
        tipul_num = tipul_num + 1
        # The next sof_tipul is the time the pet arrived and it's tipul duration
        int[i] = arrival[arrival_num]
        sof_tipul[tipul_num] = arrival[arrival_num] + tipul_duration(arrival, index = arrival_num)
        arrival_num = arrival_num+ 1
        i = i + 1
        #payment[tipul_num] = tipul_peyment_pet(names(arrival[arrival_num]))
      }
      else{
        tor_indx = tor_indx + 1
        tor[tor_indx] = tor[tor_indx - 1] + 1
        int[i] = arrival[arrival_num]
        # heres the change, defining who is the 1st customer cat or dog
        rishon_intor = names(arrival[arrival_num])
        i = i + 1
        arrival_num = arrival_num + 1
      }
    }
    
    # case when line num betwwen 1-9
    else if (tor[tor_indx] > 0 && tor[tor_indx] < 19){
      if (sof_tipul[tipul_num] < arrival[arrival_num]){
        # the tor yored 1 ,start next tipul
        tor_indx = tor_indx + 1
        payment[tipul_num] = tipul_peyment_pet(rishon_intor)
        tor[tor_indx] = tor[tor_indx - 1] - 1
        tipul_num = tipul_num + 1
        int[i] = sof_tipul[tipul_num]
        # מה הזמן של הטיפול הבא?
        # next tipul is imiddently after and the time it takes to service the rishon intor
        sof_tipul[tipul_num] = sof_tipul[tipul_num - 1] + tipul_duration_pet(rishon_intor)
        rishon_intor = "dog"
        i = i +1
      }
      
      else{
        if (names(arrival[arrival_num]) == "dog"){
          tor_indx = tor_indx + 1
          tor[tor_indx] = tor[tor_indx - 1] + 1
          arrival_num = arrival_num + 1
          int[i] = arrival[arrival_num]
          i = i +1 
        }
        # a cat has arrived' only update to the next costumer
        else{
          arrival_num = arrival_num + 1
        }
      }
      
    }
    
    #case when tor = 10 (is full)
    else{
      if (sof_tipul[tipul_num] <= arrival[arrival_num]){
        tor_indx = tor_indx + 1
        
        tor[tor_indx] = tor[tor_indx - 1] - 1
        int[i] = sof_tipul[tipul_num]
        payment[tipul_num] = tipul_peyment_pet(rishon_intor)
        tipul_num = tipul_num + 1
        # מה הזמן של הטיפול הבא?
        # next tipul is imiddently after and the time it takes to service the rishon intor
        sof_tipul[tipul_num] = sof_tipul[tipul_num - 1] + tipul_duration_pet(rishon_intor)
        rishon_intor = "dog"
        i = i +1
      }
      # a customer arrived and tor is full
      else{
        if(names(arrival[arrival_num]) == "dog"){
          dog_reject_count = dog_reject_count + 1
          arrival_num = arrival_num + 1
        }
        else{
          arrival_num = arrival_num + 1
        }
        
      }
    }
  }
  
  time_int = numeric(i - 1)
  for (j in (2:i)){
    time_int[j-1] = int[j] - int[j-1]
  }
  
  avg_estimate = avg_tor_hat(time_int, tor[1:(i-1 )])
  
  # return the avg tor 
  profit = sum(payment) - 0.1*dog_reject_count
  dog_num = sum(payment == 1)
  cat_num = sum(payment == 3)
  answer_list = list("profit" = profit, "dog_num" = dog_num,"dog_reject_count" = dog_reject_count,"cat_num" =  cat_num,
                     "est_avg_tor" = avg_estimate )
  
  #return(c(length(tor[1:(tor_indx+1)]), length(time_int),tor_indx))
  return(answer_list)
  
}

interval_2a()


sim_32a_profit = mean(replicate(100,interval_2a()$profit))
sim_32a_avg = mean(replicate(100,interval_2a()$est_avg_tor))
sim_32a_dog_num = mean(replicate(100,interval_2a()$dog_num))
sim_32a_cat_num = mean(replicate(100,interval_2a()$cat_num))
sim_32a_reject_count = mean(replicate(100,interval_2a()$dog_reject_count))

paste("The expected value of profit:" , round(sim_32a_profit,3)) 
paste("The expected value of avg tor:" , round(sim_32a_avg,3)) 
paste("The expected value of dogs served:" , round(sim_32a_dog_num ,3)) 
paste("The expected value of cats served:" , round(sim_32a_cat_num,3)) 
paste("The expected value of cat's rejected:" , round(sim_32a_reject_count,3))

interval_2b = function(){
  tipul_duration2 <- function(pet_vec, index){
    name = names(pet_vec)[index]
    if ( name == "dog"){
      serv_time = rexp(1, rate = 3.3)
    }
    else{
      serv_time = rexp(1, rate = 5.5)
    }
    return(serv_time)
  }
  
  tipul_duration_pet2 <- function(name){
    kind = name
    if ( kind == "dog"){
      serv_time = rexp(1, rate = 3.3)
    }
    else{
      serv_time = rexp(1, rate = 5.5)
    }
    return(serv_time)
  }
  
  time_dog = time_pet(3)
  time_cat <- time_pet(1.5)
  
  # adding "pet name" attribute or a data frame??
  
  names(time_dog) <-  rep("dog", length(time_dog))
  names(time_cat) <-  rep("cat", length(time_cat))
  
  # time of arrival of all costumers
  arrival  = sort(c(time_dog, time_cat))
  n = length(arrival)
  
  
  int = numeric(5*n)
  int[1] = 0
  int[2] = arrival[1]
  i = 3
  
  tor = numeric(5*n)
  tor[1] = 0
  
  
  sof_tipul = numeric(5*n)
  sof_tipul[1] = arrival[1] + tipul_duration2(arrival, index = 1)
  
  # payment of bals
  payment = numeric(5*n)
  payment[1] = tipul_peyment_pet(names(arrival[1]))
  # counter of dog rejection
  dog_reject_count = 0
  
  
  tipul_num = 1
  arrival_num = 2
  tor_indx = 1
  # first_costumer_in_line = cat or dog names of next arrival when tor == 0' 
  # only saved when arrival_time > sof_tipul
  while(int[i] < 720 & arrival_num < n - 1 ){
    # fail safe
    if (tor[tor_indx] < 0){
      tor[tor_indx] = 0
    }
    # case when tor = 0
    else if (tor[tor_indx] == 0){
      # reset the 1st in tor
      rishon_intor = c()
      if (sof_tipul[tipul_num] <= arrival[arrival_num]){
        # the tor didn't change start next tipul
        tipul_num = tipul_num + 1
        # The next sof_tipul is the time the pet arrived and it's tipul duration
        int[i] = arrival[arrival_num]
        sof_tipul[tipul_num] = arrival[arrival_num] + tipul_duration2(arrival, index = arrival_num)
        arrival_num = arrival_num+ 1
        i = i + 1
        #payment[tipul_num] = tipul_peyment_pet(names(arrival[arrival_num]))
      }
      else{
        tor_indx = tor_indx + 1
        tor[tor_indx] = tor[tor_indx - 1] + 1
        int[i] = arrival[arrival_num]
        # heres the change, defining who is the 1st customer cat or dog
        rishon_intor = names(arrival[arrival_num])
        i = i + 1
        arrival_num = arrival_num + 1
      }
    }
    
    # case when line num betwwen 1-9
    else if (tor[tor_indx] > 0 && tor[tor_indx] < 10){
      if (sof_tipul[tipul_num] < arrival[arrival_num]){
        # the tor yored 1 ,start next tipul
        tor_indx = tor_indx + 1
        payment[tipul_num] = tipul_peyment_pet(rishon_intor)
        tor[tor_indx] = tor[tor_indx - 1] - 1
        tipul_num = tipul_num + 1
        int[i] = sof_tipul[tipul_num]
        # מה הזמן של הטיפול הבא?
        # next tipul is imiddently after and the time it takes to service the rishon intor
        sof_tipul[tipul_num] = sof_tipul[tipul_num - 1] + tipul_duration_pet2(rishon_intor)
        rishon_intor = "dog"
        i = i +1
      }
      
      else{
        if (names(arrival[arrival_num]) == "dog"){
          tor_indx = tor_indx + 1
          tor[tor_indx] = tor[tor_indx - 1] + 1
          arrival_num = arrival_num + 1
          int[i] = arrival[arrival_num]
          i = i +1 
        }
        # a cat has arrived' only update to the next costumer
        else{
          arrival_num = arrival_num + 1
        }
      }
      
    }
    
    #case when tor = 10 (is full)
    else{
      if (sof_tipul[tipul_num] <= arrival[arrival_num]){
        tor_indx = tor_indx + 1
        
        tor[tor_indx] = tor[tor_indx - 1] - 1
        int[i] = sof_tipul[tipul_num]
        payment[tipul_num] = tipul_peyment_pet(rishon_intor)
        tipul_num = tipul_num + 1
        # מה הזמן של הטיפול הבא?
        # next tipul is imiddently after and the time it takes to service the rishon intor
        sof_tipul[tipul_num] = sof_tipul[tipul_num - 1] + tipul_duration_pet2(rishon_intor)
        rishon_intor = "dog"
        i = i +1
      }
      # a customer arrived and tor is full
      else{
        if(names(arrival[arrival_num]) == "dog"){
          dog_reject_count = dog_reject_count + 1
          arrival_num = arrival_num + 1
        }
        else{
          arrival_num = arrival_num + 1
        }
        
      }
    }
  }
  
  time_int = numeric(i - 1)
  for (j in (2:i)){
    time_int[j-1] = int[j] - int[j-1]
  }
  
  avg_estimate = avg_tor_hat(time_int, tor[1:(i-1 )])
  
  # return the avg tor 
  profit = sum(payment) - 0.1*dog_reject_count
  dog_num = sum(payment == 1)
  cat_num = sum(payment == 3)
  answer_list = list("profit" = profit, "dog_num" = dog_num,"dog_reject_count" = dog_reject_count,"cat_num" =  cat_num,
                     "est_avg_tor" = avg_estimate )
  
  #return(c(length(tor[1:(tor_indx+1)]), length(time_int),tor_indx))
  return(answer_list)
  
  
  
}



interval_2b()



sim_32b_profit = mean(replicate(100,interval_2b()$profit))
sim_32b_avg = mean(replicate(100,interval_2b()$est_avg_tor))
sim_32b_dog_num = mean(replicate(100,interval_2b()$dog_num))
sim_32b_cat_num = mean(replicate(100,interval_2b()$cat_num))
sim_32b_reject_count = mean(replicate(100,interval_2b()$dog_reject_count))

paste("The expected value of profit:" , round(sim_32b_profit,3)) 
paste("The expected value of avg tor:" , round(sim_32b_avg,3)) 
paste("The expected value of dogs served:" , round(sim_32b_dog_num ,3)) 
paste("The expected value of cats served:" , round(sim_32b_cat_num,3)) 
paste("The expected value of cat's rejected:" , round(sim_32b_reject_count,3))
