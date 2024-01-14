---
title:    "Clojure: Omvandla en sträng till små bokstäver"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför

Att konvertera en sträng till gemena bokstäver kan vara användbart i många situationer, som att jämföra strängar eller för att säkerställa enhetlighet i data.

## Så här gör du

För att konvertera en sträng till gemena bokstäver i Clojure, kan du använda funktionen `clojure.string/lower-case`. Den tar emot en sträng som argument och returnerar en ny sträng med alla bokstäver i små bokstäver. Till exempel:

```Clojure
(clojure.string/lower-case "HEJ") ;; => "hej"
(clojure.string/lower-case "Ha En Bra Dag") ;; => "ha en bra dag"
```

Du kan också använda `clojure.string/lower-case` för att konvertera en hel rad av text till gemena bokstäver, som i exemplet nedan:

```Clojure
(def text "Välkommen Till Clojure") ;; definerar en variabel "text" med en sträng
(clojure.string/lower-case text) ;; konverterar strängen till gemena bokstäver och returnerar "välkommen till clojure"
```

## Djupdykning

När du konverterar en sträng till gemena bokstäver i Clojure används Unicode-standarden, vilket innebär att alla unicode-tecken som har en gemensam gemena bokstav kommer att konverteras. Detta gör att funktionen `clojure.string/lower-case` är mer pålitlig och mångsidig jämfört med andra språk där konvertering av bokstäver kan vara svårt. Dessutom kan du i Clojure hantera flerspråkig text utan problem.

## Se även

Här är några användbara länkar för dig som vill lära dig mer om hur man arbetar med strängar i Clojure:

- [Clojure String Functions](https://clojuredocs.org/clojure.string)
- [Clojure for the Brave and True - Strings](https://www.braveclojure.com/basic-types/#Strings)
- [Clojure Cookbook - Working with Strings](https://clojure-cookbook.neotyk.com/strings.html#working-with-strings)