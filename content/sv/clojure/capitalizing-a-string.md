---
title:                "Att göra en sträng versal"
date:                  2024-01-19
html_title:           "Bash: Att göra en sträng versal"
simple_title:         "Att göra en sträng versal"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att kapitalisera en sträng innebär att omvandla första tecknet i varje ord till versal, det vill säga stora bokstäver. Programmerare använder detta för att formatera text på ett enhetligt sätt, exempelvis för rubriker eller namn.

## Hur gör man:
Clojure saknar en inbyggd funktion för att kapitalisera varje ord i en sträng, men det är enkelt att bygga en själv. Här är ett exempel:

```Clojure
(defn capitalize-word [word]
  (str (clojure.string/upper-case (subs word 0 1)) (subs word 1)))

(defn capitalize [s]
  (clojure.string/join " " (map capitalize-word (clojure.string/split s #"\s+"))))

(println (capitalize "hej hörni, detta är clojure!"))
```

Exempelutmatning:

```
"Hej Hörni, Detta Är Clojure!"
```

## Djupdykning
Kapitalisering av strängar är inte nytt och har använts länge inom programmering och textbehandling. I Clojure kan du kapitalisera en hel sträng med `clojure.string/capitalize`, men det påverkar bara det första ordet. För att kapitalisera alla ord måste du dela upp strängen och bearbeta varje ord individuellt. 

Ett alternativ är att använda Java-metoden `capitalize` från Apache Commons Lang biblioteket, som kan importeras med Leiningen eller Maven. Dock är det vanligt att Clojure-utvecklare bygger egen enkel funktion för att undvika externa beroenden.

Detaljer i implementeringen ovan innehåller användning av `clojure.string/upper-case` för att omvandla bokstäver till versaler och `subs` för att dela upp strängen. `map` applicerar `capitalize-word` funktionen på varje ord och `clojure.string/join` sammansätter ord till en fullständig sträng igen.

## Se även
- Clojure's built-in string functions: https://clojuredocs.org/clojure.string
- Java String Documentation for additional methods that can be used with interop: https://docs.oracle.com/javase/7/docs/api/java/lang/String.html
- Apache Commons Lang, a library that includes string utilities: https://commons.apache.org/proper/commons-lang/
