---
title:                "Clojure: Sammanslagning av strängar"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför

Att sammanfoga strängar (även kallat "att konkatenera" strängar) är en viktig del av programmering. Genom att sammanfoga olika strängar kan man skapa mer komplexa och användbara strängar. I Clojure, en funktionell programmeringsspråk, kan man enkelt sammanfoga strängar med hjälp av inbyggda funktioner.

## Så här gör du

För att sammanfoga strängar i Clojure, kan man använda sig av funktionen `str`. Den tar ett eller flera argument och returnerar en sträng som representerar de angivna argumenten, i samma ordning som de angavs. Här är ett exempel på hur man kan använda `str`-funktionen:

```Clojure
(str "Hej " "världen!") ;; Output: "Hej världen!"
```

Man kan också sammanfoga strängar med hjälp av operatorn `str`. Den tar två argument och returnerar en sammanfogad sträng. Här är samma exempel som ovan, fast med operatorn:

```Clojure
"Hej "  "världen!" ;; Output: "Hej världen!"
```

Det är också möjligt att sammanfoga strängar med `format`-funktionen. Den tar en formatsträng och ett eller flera argument och returnerar en formaterad sträng. Här är ett exempel på hur man kan använda `format` för att sammanfoga strängar:

```Clojure
(format "I %s önskar jag dig en trevlig %s." "Sverige" "dag") ;; Output: "I Sverige önskar jag dig en trevlig dag."
```

## Djupdykning

I Clojure är strängar oföränderliga (immutable), vilket betyder att de inte kan ändras efter att de har skapats. Detta innebär att varje gång man sammanfogar strängar, skapas en ny sträng istället för att ändra den ursprungliga strängen. Detta kan leda till ineffektivitet vid användning av stora datamängder, eftersom man då skapar många nya strängar istället för att ändra en befintlig.

För att undvika detta kan man använda sig av `StringBuilder`, som finns i Java-biblioteket som Clojure bygger på. `StringBuilder` låter en bygga upp en sträng bit för bit och ändra den efter behov, vilket är mer effektivt än att skapa en ny sträng varje gång.

## Se även

* [ClojureDocs - str](https://clojuredocs.org/clojure.core/str)
* [ClojureDocs - format](https://clojuredocs.org/clojure.core/format)
* [ClojureDocs - StringBuilder](https://clojuredocs.org/java.lang.StringBuilder)