---
title:    "Clojure: Utvinna delsträngar"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

# Varför

Att extrahera substrängar är en användbar teknik inom Clojure-programmering eftersom det gör det möjligt att manipulera textsträngar på ett mer effektivt sätt. Genom att extrahera substrängar kan du också få tillgång till specifika delar av en textsträng, vilket kan vara användbart för olika programmeringsuppgifter.

## Så här gör du

Extrahera en del av en textsträng genom att använda ```subs``` funktionen. Till exempel, om du vill extrahera de första fyra tecknen från en textsträng, kan du använda följande kod:

```Clojure
(def text "Hej världen!")
(subs text 0 4)
```
Detta kommer att producera outputen "Hej ", eftersom strängen "Hej världen!" börjar på index 0 och slutar på index 3 (eftersom det fjärde tecknet inte är inkluderat).

Om du vill extrahera en del av strängen baserat på ett visst tecken kan du använda ```subs``` funktionen tillsammans med ```clojure.string/index-of``` funktionen. Till exempel, om du vill extrahera allt efter tecknet "världen!" från en sträng, kan du använda följande kod:

```Clojure
(def text "Hej världen!")
(subs text (+ (clojure.string/index-of text "världen!") 1))
```
Detta kommer att producera outputen "!", eftersom det är allt som finns kvar av strängen efter "världen!".

## Djupdykning

När du extraherar substrängar kan du också använda dig av regular expressions (regex) för att matcha vissa mönster i strängar och extrahera dessa delar. Detta är särskilt användbart om du vill extrahera delar av en sträng som följer ett specifikt format eller mönster.

Till exempel, om du vill extrahera texten mellan två parentheser från en sträng, kan du använda följande kod:

```Clojure
(def text "Jag älskar (att) programmera")
(clojure.string/replace text #"\((.*)\)" "$1")
```
Detta kommer att producera outputen "att", eftersom det är det som finns mellan parentheserna i strängen.

## Se även

- [Officiell Clojure dokumentation för ```subs``` funktionen](https://clojuredocs.org/clojure.core/subs)
- [En guide till regex inom Clojure](https://www.martinhaye.dk/clojure/regex-tutorial/)