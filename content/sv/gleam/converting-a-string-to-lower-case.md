---
title:                "Gleam: Omvandla en sträng till gemener"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför

Att konvertera en sträng till små bokstäver är ett vanligt uppgift inom programmering, och kan vara användbart i många olika situationer. Det kan hjälpa till att göra sträng jämförelsen mer exakt och lättare att hantera, eller det kan vara en del av en större process för att formatera data.

## Hur man gör det

Att konvertera en sträng till små bokstäver i Gleam är enkelt och kan göras med hjälp av en inbyggd funktion, `String.to_lower_case`. Här är ett exempel på hur man använder denna funktion:

```Gleam
let original_sträng = "HEJ VÄRLDEN"
let konverterad_sträng = String.to_lower_case(original_sträng)
```

Resultatet av detta skulle vara `hej världen`, där alla bokstäver har omvandlats till små bokstäver. Det är viktigt att notera att detta också gäller för icke-latinska bokstäver och specialtecken. Till exempel skulle `ÄÄÄÖ` konverteras till `ääääö`.

## Djupdykning

För att förstå hur funktionen `String.to_lower_case` fungerar i Gleam, är det viktigt att förstå Unicode-tecknen. I Unicode-systemet har varje tecken en unik kodpunkt, vilket möjliggör representativitet av alla språk och skriftsystem.

När vi använder funktionen `String.to_lower_case` i Gleam, går datorn igenom varje tecken i strängen och kontrollerar dess Unicode-kodpunkt. Om tecknet finns i den stora bokstavsutförande, omvandlas det till motsvarande små bokstav. Om tecknet inte finns i den stora bokstavsutförande, lämnas det oförändrat.

## Se även

- [Gleam dokumentation om strängar](https://gleam.run/documentation/stdlib/strings/)
- [Unicode definition](https://unicode.org/)