---
title:                "Gör om en sträng till versaler"
html_title:           "Clojure: Gör om en sträng till versaler"
simple_title:         "Gör om en sträng till versaler"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Vad och varför?

Att kapitalisera en sträng innebär att man ändrar de första bokstäverna i varje ord till stora bokstäver. Programmerare gör detta för att förbättra läsbarheten och presentera data på ett mer formellt sätt.

## Hur man gör:

Använd `clojure.string/capitalize` funktionen från Clojure biblioteket. Här är ett exempel:

```Clojure
(require '[clojure.string :as str])

(defn capitalize-str [s]
    (str/capitalize s))

(println (capitalize-str "hej världen"))
```

Den producerar följande output:

```Clojure
"Hej Världen"
```

## Djupdykning

Historiska sammanhang: Funktionen capitalized från Clojure's `clojure.string` biblioteket används ofta för att bearbeta och presentera textdata.

Alternativ: Det finns andra sätt att kapitalisera en sträng, såsom att använda `clojure.string/upper-case` funktionen, men den kommer att göra alla bokstäver stora, inte bara den första bokstaven.

Detaljer om implementeringen: Funktionen `clojure.string/capitalize` är en högre ordningens funktion som tar en sträng som indata och returnerar en ny sträng där varje ordstartsbokstav är stor.

## Se också

1. [Clojure Programmering](https://clojuredocs.org/clojure.string/capitalize) Här kan du läsa mer om `capitalize` funktionen.

2. [Källkod för clojure.string/capitalize](https://github.com/clojure/clojure/blob/master/src/clj/clojure/string.clj) här hittar du den faktiska implementeringen i Clojure's källkod.

3. [Alternativ för strängmanipulation i Clojure](https://clojuredocs.org/quickref/Clojure%20Core/string) en samling av andra användbara metoder för att manipulera strängar i Clojure.