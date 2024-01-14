---
title:    "Gleam: Jämföring av två datum"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför

Att jämföra två datum kan vara en vanlig uppgift i många program. Det kan hjälpa dig att avgöra om ett visst datum ligger före eller efter ett annat, eller om de är samma datum.

## Så här gör du

För att jämföra två datum i Gleam, kan du använda funktionen `Date.compare`. Det finns tre möjliga utfall av denna funktion: `Less`, `Equal` eller `Greater`. Dessa representerar om det första datumet är mindre än, lika med eller större än det andra datumet.

För att använda funktionen behöver du skapa två `Date`-objekt och sedan använda dem som argument i `Date.compare`-funktionen.

```Gleam
let startDate = Date.from_iso_8601("2021-01-01")
let endDate = Date.from_iso_8601("2021-01-30")

let result = Date.compare(startDate, endDate)
```

I det här exemplet kommer `result` att bli `Less`, eftersom `startDate` ligger tidigare än `endDate`. Du kan också använda operatorn `<`, `=` eller `>` för att jämföra direkt mellan två datum.

## Djupdykning

Vid jämförelse av datum är det viktigt att förstå skillnaden mellan lokala och UTC-tider. En lokal tid är baserad på tidszonen där användaren befinner sig, medan UTC-tider är baserade på Universal Time Coordinated.

När du skapar `Date`-objekt i Gleam, skapas de automatiskt som UTC-tider. Detta kan påverka resultatet av en jämförelse om du inte tar hänsyn till tidszonen.

För att hantera detta kan du använda funktionen `Date.with_timezone` för att omvandla ett `Date`-objekt till en lokal tid baserad på en specifik tidszon.

```Gleam
let localDate = Date.with_timezone(startDate, Timezone.from_name("Europe/Stockholm"))
```

## Se även

Här är några användbara länkar för mer information om att jämföra datum i Gleam:

- [Officiell dokumentation för Date-modulen](https://gleam.run/modules/date.html#Date.compare)
- [Blogginlägg om att jämföra datum i Gleam av The Gleam Experiments](https://gleampowered.org/Comparing-dates-in-Gleam)