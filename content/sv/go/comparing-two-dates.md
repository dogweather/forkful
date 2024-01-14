---
title:    "Go: Jämförande av två datum"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Varför?

När vi utvecklar program i Go är det vanligt att vi behöver jämföra olika datum för att utföra vissa åtgärder eller beräkningar. I den här bloggposten kommer vi att ta en titt på hur man kan jämföra två datum i Go och utforska några av de inbyggda funktionerna som finns tillgängliga för detta ändamål.

## Så här gör du

För att jämföra två datum i Go, behöver vi först skapa två variabler som innehåller de datum som vi vill jämföra. Vi kan använda det inbyggda paketet time för att skapa dessa variabler. Låt oss titta på ett exempel där vi jämför två datum för att se om det första datumet är före det andra.

```
// Skapar två variabler med olika datum
date1 := time.Date(2020, time.June, 1, 0, 0, 0, 0, time.UTC)
date2 := time.Date(2021, time.January, 1, 0, 0, 0, 0, time.UTC)

// Jämför de två datumen
if date1.Before(date2) {
  fmt.Println("Datum 1 är före datum 2")
} else {
  fmt.Println("Datum 1 är efter datum 2")
}

// Output:
// Datum 1 är före datum 2

```

I det här exemplet skapar vi två variabler, date1 och date2, med olika datum. Sedan använder vi funktionen Before() från paketet time för att jämföra de två datumen. Om date1 är före date2 kommer vi att få utskriften "Datum 1 är före datum 2" annars kommer vi att få "Datum 1 är efter datum 2". Det finns också andra funktioner i paketet time som används för att jämföra datum som After(), Equal() och Compare().

## Djupdykning

När vi jämför datum i Go finns det vissa saker som vi behöver vara medvetna om. Till exempel kan det hända att två datum som faktiskt är lika inte kommer att betraktas som lika när de jämförs på grund av skillnader i tidszoner eller precision. För att undvika sådana problem kan vi använda funktionen Truncate() för att ta bort eventuell precision från datumen innan vi jämför dem.

## Se även

- Här hittar du mer information om paketet time i Go: https://golang.org/pkg/time/
- Om du vill lära dig mer om datum och tidsberäkningar i Go, se den här artikelserie: https://www.calhoun.io/working-with-dates-and-times-in-go/
- En bra övning är att försöka jämföra datum i olika scenarier och se hur tidszoner eller precision kan påverka resultatet.