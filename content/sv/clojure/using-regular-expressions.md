---
title:                "Att använda reguljära uttryck"
html_title:           "Clojure: Att använda reguljära uttryck"
simple_title:         "Att använda reguljära uttryck"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Varför

Det finns många anledningar till varför man skulle vilja använda reguljära uttryck (regular expressions). De kan användas för att söka, extrahera och manipulera text på ett kraftfullt sätt. De är också användbara för att hantera validering av inmatning och hantering av data.

## Så här använder du reguljära uttryck i Clojure

Det första du behöver göra är att importera Clojure's regex-bibliotek. Detta gör du genom att lägga till följande kod längst upp i ditt program:

```Clojure
(use 'clojure.string)
```

För att sedan söka efter ett mönster i en sträng kan du använda funktionen `re-find`. Till exempel, om du vill hitta alla ord som börjar med bokstaven "a" i en sträng, skulle koden se ut så här:

```Clojure
(re-find #"a\w+" "Hej alla a-böcker")
```

Resultatet skulle bli en lista med alla matchande ord ("alla" och "a-böcker" i vårt exempel). Notera att vi använder `#"a\w+"` för att definiera vårt sökmönster. Den första delen, `#`, visar att vi använder ett reguljärt uttryck. Sedan följer mönstret som vi vill matcha, i detta fall "a" följt av ett eller flera bokstäver `\w+`.

Du kan också använda reguljära uttryck för att extrahera information från en sträng. Till exempel, om du har en sträng med olika format för telefonnummer kan du använda följande kod för att extrahera endast siffrorna:

```Clojure
(re-find #"\d+" "(123) 456-7890")
```

Resultatet skulle bli 1234567890.

## Djupdykning

Det finns många andra funktioner för reguljära uttryck i Clojure som du kan utforska. Här är några att börja med:

- `re-seq`: används för att hitta alla matchningar i en sträng och returnerar dem som en sekvens.
- `re-matches`: används för att hitta en exakt matchning, till skillnad från `re-find` som kan hitta delmönster.
- `re-first`: returnerar den första matchningen, användbart om du bara är intresserad av den första matchningen.
- `re-pattern`: används för att konvertera en sträng till en reguljär uttrycks-mall.

Se till att läsa dokumentationen för Clojure regex-bibliotek (länk nedan) för att lära dig mer om alla tillgängliga funktioner.

## Se också

- [Clojure Regex Biblioteket Dokumentation](https://clojure.github.io/clojure/clojure.string-api.html#clojure.string/re-find)
- [Reguljära uttryck Cheat Sheet](https://www.rexegg.com/regex-quickstart.html)