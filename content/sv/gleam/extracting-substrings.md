---
title:                "Extrahera delsträngar"
html_title:           "Gleam: Extrahera delsträngar"
simple_title:         "Extrahera delsträngar"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför

Om du har jobbat med strängar i andra programmeringsspråk, som Javascript eller Java, har du kanske stött på behovet av att extrahera en del av en sträng. Med Gleam kan du göra detta enkelt genom att använda den inbyggda funktionen `String.slice` för att skapa en understräng av en befintlig sträng.

## How To

För att använda `String.slice` och få en delsträng behöver du ange index för början och slutet av den delsträng du vill extrahera. Till exempel, om du har en sträng som heter `namn` och vill extrahera bokstäverna 3 till 6, skulle din kod se ut så här:

```Gleam
namn = "Gustav"
delsträng = String.slice(namn, 3, 6)
```

Outputen av detta skulle vara `sta`, vilket är bokstäverna 3 till 6 av strängen `namn`. Om du endast anger ett argument, t.ex `String.slice(namn, 2)`, kommer den delsträngen att börja på index 2 och gå till slutet av strängen.

Det är också möjligt att använda negativa index för bakåtriktad räkning. Om du vill extrahera de två sista bokstäverna av strängen `namn` skulle koden se ut som följer:

```Gleam
sista_delsträng = String.slice(namn, -2, -1)
```

Outputen av detta skulle då bli `va`.

## Deep Dive

Utöver att använda `String.slice`, finns det också andra sätt att extrahera substrängar i Gleam. Du kan använda funktionen `String.split` för att dela upp en sträng baserat på ett angivet separator-tecken. Till exempel, om du vill dela upp strängen `färg` vid tecknet `-` skulle koden se ut som följer:

```Gleam
färger = "röd-blå-grön"
delar = String.split(färger, "-")
```

Outputen av detta skulle vara en lista med tre element: `["röd", "blå", "grön"]`.

Det är också möjligt att använda regex i Gleam för att extrahera substrängar. Funktionen `Regex.captures` returnerar en lista med matcher baserat på ett angivet regex uttryck. Till exempel, om du vill hitta alla ord som börjar med bokstaven `s` i strängen `text` skulle koden se ut så här:

```Gleam
text = "sol, sand, snö, sjö"
matcher = Regex.captures(text, ~r/s[a-z]*/)
```

Outputen av detta skulle vara en lista med matcher: `["sol", "sand", "snö", "sjö"]`.

## See Also

För att lära dig mer om Gleam och dess funktioner, kan du besöka följande länkar:

- [Officiell Gleam dokumentation](http://gleam.run)
- [Gleam - Ett nytt programmeringsspråk för funktionell programmering](https://medium.com/@lpil/introducing-gleam-a-new-programming-language-for-functional-programming-3a8c6a602a21)