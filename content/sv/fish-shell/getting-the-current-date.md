---
title:                "Fish Shell: Att få nuvarande datum."
programming_language: "Fish Shell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför

Att få den aktuella datumet kan vara en viktig del av många programmeringsprojekt. Oavsett om du vill ha det för att logga händelser, spåra tidsstämplar eller för en annan anledning, är det viktigt att veta hur man kan göra detta i Fish Shell.

## Hur man gör det

Att få den aktuella datumet i Fish Shell är enkel och kan göras med ett fåtal kommandon. Först måste du öppna Fish Shell i din terminal och sedan använda "echo" kommandot tillsammans med "date" för att få datumet. Detta kan göras enligt följande:

```Fish Shell 
echo (date)
```

Detta kommer att skriva ut den aktuella datumet i formatet "Dag, Månad DD TT:MM:SS ÅÅÅÅ".

## Deep Dive

Det finns olika sätt att anpassa hur datumet visas i Fish Shell beroende på dina behov. Till exempel kan du lägga till olika flaggor vid "date" kommandot för att ändra formatet på datumet. Några vanliga flaggor inkluderar "-r" för att få den relativa tiden, "-f" för att ange önskat format och "-u" för att få UTC-tid istället för lokal tid.

Det är också möjligt att använda variabler och funktioner för att få mer avancerad information om datumet, som veckodag, veckonummer och årskalender. För att läsa mer om dessa möjligheter och andra användbara tips, kan du kolla in Fish Shells online-dokumentation och guider.

## Se även

Här är några användbara länkar för att lära dig mer om att få datumet i Fish Shell:

- Fish Shell Dokumentation: https://fishshell.com/docs/current/index.html
- Fish Shell Cookbok: https://fishshell.com/docs/current/cookbook.html#getting-the-current-date-and-time
- Fish Stack Exchange: https://fishshell.com/docs/current/index.html