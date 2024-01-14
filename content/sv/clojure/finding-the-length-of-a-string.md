---
title:                "Clojure: Hitta längden på en sträng"
simple_title:         "Hitta längden på en sträng"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att Kunna hitta längden av en sträng är en nyckelfunktion inom många programmeringsspråk, inklusive Clojure. Genom att lära dig hur man gör detta i Clojure kan du öka din förmåga att manipulera text och göra mer avancerade program.

## Hur man gör

För att hitta längden av en sträng i Clojure använder vi funktionen `count`, som tar en sekvens som argument och returnerar dess längd. Här är ett exempel som visar hur man kan använda `count` för att hitta längden på en sträng:

```Clojure
(def sträng "Hej, världen!")
(count sträng) ; output: 13
```

Här kan vi se att funktionen `count` returnerar längden på strängen "Hej, världen!" som är 13 tecken.

Vi kan också använda `count` för att hitta längden på en lista eller struktur. Till exempel:

```Clojure
(def lista '(1 2 3 4 5))
(count lista) ; output: 5
```

Det är viktigt att komma ihåg att `count` bara kan användas på sekvenser som implementerar gränssnittet `ISeq` eller `ILookup`. Detta inkluderar bland annat strängar, listor, vektorer och kartor.

## Djupdykning

En intressant sak att notera är att i Clojure har sekvenser ingen inbyggd längd egenskap, men istället använder de funktionen `count`. Detta tillåter olika typer av sekvenser att ha olika sätt att beräkna sin längd, vilket gör att vi kan hantera dem på ett mer flexibelt sätt.

En annan viktig aspekt att notera är att `count` är en så kallad "lazy" funktion i Clojure, vilket innebär att den bara beräknar längden av sekvensen på begäran. Detta gör att vi kan använda det på oändliga sekvenser utan att orsaka en oändlig loop.

## Se även

- [Clojure - count function](https://clojuredocs.org/clojure.core/count)
- [Clojure - strings](https://clojure.org/reference/strings)
- [Clojure - sequences](https://clojure.org/reference/sequences)