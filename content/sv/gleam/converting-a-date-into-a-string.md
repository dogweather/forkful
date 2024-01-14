---
title:    "Gleam: Konvertera ett datum till en sträng"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Varför
Det kan finnas många olika anledningar till varför man skulle vilja konvertera ett datum till en sträng i Gleam. Det kan vara för att presentera användarvänlig information, skapa rapporter eller kanske för att spara informationen i en databas.

## Hur man gör det
Det kan verka som en svår uppgift, men med hjälp av Gleams inbyggda funktioner och regex-uttryck är det faktiskt ganska enkelt. Nedan följer ett exempel på hur man kan konvertera ett datum till en sträng i Gleam:

```Gleam
import gleam/datetime.Date

let date = Date.parse(~date="2021-05-12")

let string_date = Date.to_string(date, ~format="dd/mm/yyyy")

IO.print(string_date)
```

Output: "12/05/2021"

## Djupdykning
Det finns flera olika format och alternativ för hur man kan konvertera ett datum till en sträng i Gleam. Man kan använda sig av olika format, vilket i exemplet ovan var "dd/mm/yyyy", där "dd" står för dag, "mm" för månad och "yyyy" för år. Det finns också möjlighet att ange tidszon och andra inställningar för att få en mer exakt representation av datumet.

En annan intressant aspekt är att Gleam's Date-modul också tillåter oss att manipulera datumet innan vi konverterar det till en sträng. Det kan till exempel vara användbart om man behöver presentera datumet i en specifik tidszon.

## Se även
- [Gleam Date-modul dokumentation](https://gleam.run/documentation/standard_library/datetime)
- [Regex tutorial för Gleam](https://dev.to/yaminmhd/regex-tutorial-gleam-part-1-4g96)