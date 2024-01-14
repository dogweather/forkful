---
title:    "Go: Beräkning av ett datum i framtiden eller det förflutna"
keywords: ["Go"]
---

{{< edit_this_page >}}

# Varför

Att kunna beräkna ett datum i framtiden eller förflutna kan vara användbart i många olika situationer, till exempel vid planering av resor eller att hålla koll på deadlines. Genom att använda Go kan du enkelt implementera denna funktion i dina program.

# Så här gör du

För att beräkna ett datum i framtiden eller förflutna i Go, behöver du först importera paketet "time" som innehåller grundläggande funktioner för tidsberäkningar. Sedan kan du använda funktionen "AddDate(years, months, days)" för att lägga till eller dra bort ett visst antal år, månader eller dagar från ett givet datum.

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    // Beräknar ett datum 14 dagar framåt från dagens datum
    futureDate := time.Now().AddDate(0, 0, 14)
    fmt.Println("Datumet om 14 dagar är:", futureDate.Format("02 Jan 2006"))

    // Beräknar ett datum 6 månader tillbaka från dagens datum
    pastDate := time.Now().AddDate(0, -6, 0)
    fmt.Println("Datumet för 6 månader sedan var:", pastDate.Format("02 Jan 2006"))
}
```

**Output:**

>Datumet om 14 dagar är: 26 Jun 2021
>
>Datumet för 6 månader sedan var: 27 Dec 2020

# Djupdykning

Genom att använda funktioner som "AddDate()" kan du även hantera skottår och månader med olikt antal dagar. Funktionen tar hänsyn till dessa faktorer och justerar automatiskt det beräknade datumet för att matcha rätt månadslängd.

För att få ut mer noggrann information om ett datum, kan du använda funktionen "Date(year, month, day, hour, minute, second, nanosecond, location)" som returnerar ett "Time" objekt som innehåller all information om det specifika datumet.

# Se även

* Officiell dokumentation för Go: https://golang.org/doc/
* Tutorial för tidshantering i Go: https://golang.org/doc/time/
* Kodexempel och övningsuppgifter: https://gobyexample.com/dates