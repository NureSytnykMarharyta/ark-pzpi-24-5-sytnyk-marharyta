// рахуємо
let calc(l:list<decimal*int>)=
  let mutable t=0m
  for (p,q) in l do t<-t+p*decimal q
  if t>1000m then t*0.9m*1.2m else t*1.2m
