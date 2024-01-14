---
title:    "Elm: Omvandla ett datum till en sträng"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att konvertera en datum till en sträng är en viktig del av många program, särskilt när det gäller att visa datumet på ett läsbart sätt. Med Elm kan man enkelt konvertera datum till strängar och på så sätt förbättra användarupplevelsen i sina appar.

## Hur man gör det

För att konvertera ett datum till en sträng i Elm använder man funktionen `DateTime.toString`. Här är ett exempel på hur man kan använda den:

```Elm
DateTime.toString "2021-09-15" -- Resultat: "15/09/2021"
```

Man kan också ange ett visst datumformat som argument i funktionen för att få önskat format på strängen. Till exempel:

```Elm
DateTime.fromDate 2031 5 4
    |> Maybe.andThen DateTime.toString WithDate
    -- Resultat: "04/05/2031"
```

Det finns olika formatalternativ som man kan ange, som "monthName" för att få månadens namn istället för siffran. Detta kan vara användbart om man vill visa datum på ett mer visuellt sätt för användaren.

```Elm
DateTime.toString "2021-09-15" { monthName = True } -- Resultat: "15 September 2021"
```

## Djupdykning

För att förstå hur konverteringen från datum till sträng fungerar i Elm, kan man titta på hur datatyperna är strukturerade. Ett datum i Elm representeras av en `DateTime` som består av en årtal, en månad och en dag. För att konvertera till en sträng används sedan dessa delar för att skapa den slutgiltiga strängen med önskad formatmall.

Det är också viktigt att hålla i minnet att konverteringen från en sträng till ett datum också är möjlig i Elm med hjälp av funktionen `DateTime.fromString`.

## Se även

- Officiell dokumentation för `DateTime` i Elm: https://package.elm-lang.org/packages/elm-lang/core/latest/DateTime
- Stack Overflow-fråga om konvertering av datum till sträng i Elm: https://stackoverflow.com/questions/64296075/how-to-convert-utc-time-to-date-with-elm