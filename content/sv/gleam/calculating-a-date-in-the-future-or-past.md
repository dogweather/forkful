---
title:    "Gleam: Beräkna ett datum i framtiden eller det förflutna"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Varför

Det finns många olika tillfällen då man kan behöva beräkna ett datum i framtiden eller i det förflutna. Kanske planerar du en resa och vill veta vilket datum du behöver återvända för att hinna med ett viktigt möte, eller kanske vill du veta när du fyller jämnt om några år. Oavsett anledningen, så kan Gleam hjälpa dig att enkelt beräkna detta datum.

## Hur man gör det

För att beräkna ett datum i framtiden eller förflutna i Gleam, använder man funktionen `Date.add` eller `Date.sub` tillsammans med ett antal dagar, veckor, månader eller år man vill gå framåt eller tillbaka från det datum man utgår ifrån.

```Gleam
let start_date = Date.new(2020, 07, 15)
let future_date = Date.add(2, "years", start_date)
// future_date kommer att vara den 15 juli 2022

let past_date = Date.sub(6, "months", start_date)
// past_date kommer att vara den 15 januari 2020
```

Som du kan se i exemplen ovan, är det enkelt att beräkna ett datum i framtiden eller förflutna i Gleam. Du kan även göra beräkningar med veckor, månader eller år istället för enbart dagar. Gleam har också stöd för att beräkna skottår, så att du kan vara säker på att dina datumberäkningar är korrekta.

## Djupdykning

För de som vill gå ännu djupare in i ämnet, så finns det en mängd olika variationer och specialfall som man kan hantera med Date-modulen i Gleam. Man kan till exempel bara beräkna arbetsdagar istället för kalenderdagar, eller ta hänsyn till olika tidszoner vid beräkningar. Det finns också möjlighet att formatera outputen på olika sätt, beroende på vilket format man behöver det i.

## Se även

- Officiell Gleam-hemsida: https://gleam.run/
- Gleam-dokumentation: https://gleam.run/documentation/
- Gleam Date-modul: https://gleam.run/modules/Date.html