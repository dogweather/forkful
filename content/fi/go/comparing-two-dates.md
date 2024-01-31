---
title:                "Kahden päivämäärän vertailu"
date:                  2024-01-20T17:33:04.982515-07:00
model:                 gpt-4-1106-preview
simple_title:         "Kahden päivämäärän vertailu"

category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why?
Vertaillaan kahta päivämäärää. Tarvitaan, kun halutaan tietää ajan kulku, vanheneminen tai deadlinejen hallinta.

## How to:
Go-kielessä `time`-paketti on avainasemassa. Vertaillaan kaksi `time.Time` -tyyppistä:

```go
package main

import (
    "fmt"
    "time"
)

func main() {
    date1 := time.Date(2023, 3, 14, 0, 0, 0, 0, time.UTC)
    date2 := time.Now()
    
    // Onko date1 ennen date2?
    if date1.Before(date2) {
       fmt.Println("date1 on ennen date2")
    }
	
    // Onko date1 jälkeen date2?
    if date1.After(date2) {
       fmt.Println("date1 on jälkeen date2")
    }

    // Vai ovatko ne samat?
    if date1.Equal(date2) {
       fmt.Println("date1 ja date2 ovat samat")
    }
}
```

Esimerkin tulostus:

```
date1 on ennen date2
```

## Deep Dive
Comparing dates on hyvin yleinen operaatio historiasta lähtien. `time`-paketin funktio `Before`, `After` ja `Equal` tekevät vertailun idiomaattisesti.

Unix-aikakausi alkoi 1. tammikuuta 1970, mikä on usein käytetty vertailukohta. Go:n `time`-paketti käsittelee leimaa `Unix()`-metodilla.

Yksinkertainen vertailu riittää usein, mutta jos tarvitaan enemmän logiikkaa, voi käyttää `Sub`-metodia, joka antaa `time.Duration`-tyypin, tai jopa `Since`- ja `Until`-metodeja.

## See Also
- Go 'time' package documentation: https://pkg.go.dev/time
- Vikkelä tutustuminen aikaleimoihin Go:ssa: https://yourbasic.org/golang/time-change-convert-format/
- Unix-aika, joka on yhteinen käytäntö ajanhetkien tallentamiselle: https://en.wikipedia.org/wiki/Unix_time
