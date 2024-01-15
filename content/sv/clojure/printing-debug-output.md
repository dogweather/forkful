---
title:                "Utskrift av felsökningsutdata"
html_title:           "Clojure: Utskrift av felsökningsutdata"
simple_title:         "Utskrift av felsökningsutdata"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför?

Ibland när du arbetar med Clojure, kan du stöta på problem som du behöver felsökning för att lösa. I dessa situationer kan det hjälpa att använda "utskrift" för att spåra och hitta felaktig kod. Det kan också vara en bra metod för att förstå hur en viss del av din kod fungerar.

## Hur man gör det

För att skriva ut felsökningsutdata i Clojure, använd "println" funktionen. Detta gör att du kan skriva ut en sträng eller ett värde till din konsol. Det finns också andra specialiserade funktioner såsom "prn" och "pprint" som ger ut mer läsbara utdata.

```
Clojure
(let [x 5]
  (println x)) ;; utdata: 5
```

## Deep Dive

När du använder "println" eller liknande funktioner, ska du vara medveten om värdenas utformning när de skrivs ut. Till exempel om du skriver ut en vektor eller map, ska du komma ihåg att värdena kommer att skrivas ut med parenteser runt sig. Detta kan hjälpa dig att identifiera problem som kanske finns i din datastruktur.

## Se även

- Clojure.org - https://clojure.org/
- Clojure cheatsheet - https://clojure.org/api/cheatsheet
- Repl.it - https://repl.it/languages/clojure