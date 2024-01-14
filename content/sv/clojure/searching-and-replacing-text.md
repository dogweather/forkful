---
title:                "Clojure: Sökning och ersättning av text"
simple_title:         "Sökning och ersättning av text"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Varför
En vanlig uppgift inom programmering är att söka och ersätta text i en given fil eller sträng. Det kan bero på att man vill korrigera stavfel, byta ut vissa ord eller helt enkelt göra en snabb ändring i en stor mängd text. Men varför skulle man behöva göra det i Clojure?

## How To

Sök och ersätt funktionen i Clojure är enkel och kraftfull tack vare användningen av reguljära uttryck. För att söka efter en viss textsträng i en fil eller sträng använder man sig av `re-seq` funktionen tillsammans med ett reguljärt uttryck och den text man vill söka i. Till exempel om vi vill hitta alla förekomster av ordet "häst" i en fil kan vi använda följande kod:

```Clojure
(def fil-innehåll (slurp "minfil.txt"))  ; läser in hela filens innehåll som en sträng
(re-seq #"\bhäst\b" fil-innehåll) ; söker efter ordet häst i filinnehållet
```
Output:
```
("häst" "häst" "häst") ; antal förekomster av ordet "häst"
```
För att ersätta alla förekomster av ordet "häst" med "katt" kan man använda sig av `re-find` tillsammans med `re-replace` funktionen. Detta gör att vi kan söka efter en textsträng och samtidigt byta ut den. Koden skulle se ut så här:

```Clojure
(re-replace #"häst" "katt" fil-innehåll)
```
Output:
```
"katt äter morötter och gräs" ; ursprungliga filinnehållet med alla förekomster av "häst" bytta mot "katt"
```
I exempelkoden ovan har vi använt `#` före vårt reguljära uttryck, detta betyder att vi skapar en reguljär uttrycksliteral som är en effektivare och snabbare metod jämfört med `re-find` som tar emot en regexp-sträng.

## Deep Dive
I Clojure kan man också använda sig av `re-pattern` funktionen för att kompilera en regexp-sträng till ett pattern objekt. Detta kan skapa en prestandaförbättring om man behöver söka efter samma reguljära uttryck flera gånger. Man kan också använda sig av `re-matcher` funktionen för att hitta ytterligare information om den sökta texten, till exempel vilken position i texten den finns på.

## Se också
- [The Clojure cheat sheet](https://clojure.org/api/cheatsheet): En snabb referensguide för Clojure.
- [ClojureDocs](https://clojuredocs.org/): En online samling av Clojure dokumentation och exempel.
- [Mastering Regular Expressions](https://regex.info/book.html): En guide för att lära sig reguljära uttryck på djupet.