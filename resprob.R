resprob <- function(att.ht, def.ht, att.at, def.at, intcept)
    {
        hwin = 0
        awin = 0
        tie = 0
        for (i in 0:5)
            {
                for (j in 0:5)
                    {
                        prob = dpois(i, exp(intcept + att.ht + def.at)) * dpois(j, exp(intcept + att.at + def.ht))
                        cat(i, ":",  j, "   ")
                        print(prob)
                        if (i < j)
                            {
                                awin = awin + prob
                            }
                        else if(i > j)
                            {
                                hwin = hwin + prob
                            }
                        else
                            {
                                tie = tie + prob
                            }
                    }
            }
        print(hwin)
        print(tie)
        print(awin)
    }
