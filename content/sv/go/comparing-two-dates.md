---
title:                "Go: Jämförande av två datum"
programming_language: "Go"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Varför

Om du är ny inom programmering och precis har börjat lära dig Go, kan du undra varför det är viktigt att kunna jämföra två datum i ett program. Att kunna hantera datum och tid är en viktig del av många applikationer och det är därför viktigt att ha en god förståelse för hur man jämför och arbetar med datum i Go.

# Hur man gör det

För att jämföra två datum i Go kan du använda funktionen `Before` eller `After` från paketet `time`. Dessa funktioner tar två datum som argument och returnerar en boolean som indikerar om det första datumet är före eller efter det andra. Här är ett exempel på hur man skulle använda dessa funktioner:

```
Go func main() {
    firstDate := time.Date(2021, time.July, 15, 0, 0, 0, 0, time.UTC)
    secondDate := time.Date(2021, time.July, 20, 0, 0, 0, 0, time.UTC)

    if firstDate.Before(secondDate) {
        fmt.Println("Första datumet är före det andra")
    } else {
        fmt.Println("Andra datumet är före det första")
    }
}
```

I detta exempel skapar vi två datum med funktionen `Date` från paketet `time` och sedan använder vi `Before` för att jämföra dem. Eftersom första datumet är tidigare än det andra, kommer utskriften att vara "Första datumet är före det andra". Du kan också använda `After` funktionen på samma sätt för att jämföra två datum.

# Djupdykning

Om du vill ha en mer detaljerad jämförelse av två datum kan du också använda funktionen `Equal` från paketet `time`. Detta jämför båda datumets år, månad och dag och returnerar en boolean som indikerar om de är lika. Här är ett exempel på hur man skulle använda den:

```
Go func main() {
    firstDate := time.Date(2021, time.July, 15, 0, 0, 0, 0, time.UTC)
    secondDate := time.Date(2021, time.July, 15, 0, 0, 0, 0, time.UTC)

    if firstDate.Equal(secondDate) {
        fmt.Println("Båda datumen är samma")
    } else {
        fmt.Println("Datumen är inte samma")
    }
}
```

I detta exempel kommer utskriften att vara "Båda datumen är samma" eftersom både år, månad och dag är samma för båda datumen.

# Se också

- [Golang Time Package Documentation](https://pkg.go.dev/time)
- [A Guide to Implementing Time and Date in Go](https://blog.golang.org/examples-time)
- [Understanding Date and Time in Go](https://www.calhoun.io/working-with-date-and-time-in-go/)