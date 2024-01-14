---
title:    "Clojure: Kontroll av befintlig katalog"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför

När man arbetar med Clojure, kan man ibland behöva kontrollera om en mapp (directory) finns eller inte. Detta kan vara användbart för att göra vissa åtgärder beroende på om mappen redan finns, eller om man behöver skapa den. I denna bloggpost kommer jag att förklara hur man kan göra detta i Clojure.

## Hur man gör

För att kontrollera om en mapp finns eller inte, kan man använda funktionen "exists?" från biblioteket "clojure.java.io". Först behöver man importera biblioteket genom att lägga till följande rad i början av sin kod:

```Clojure
(require '[clojure.java.io :as io])
```

Sedan kan man använda sig av "exists?"-funktionen och ange en sträng med sökvägen till mappen som argument. Om mappen finns kommer funktionen att returnera "true", annars kommer den att returnera "false". Detta kan sedan användas som en boolesk variabel i villkorssatser.

Låt oss se ett exempel:

```Clojure
(define directory "min-mapp")
(if (io/exists? directory)
  (println "Mappen finns redan")
  (println "Mappen finns inte, skapar den..."))
```

Om "min-mapp" redan finns kommer utskriften att vara "Mappen finns redan", annars kommer utskriften att vara "Mappen finns inte, skapar den...".

## Djupdykning

För att förstå hur "exists?"-funktionen fungerar, kan vi titta på dess implementation i "clojure.java.io" biblioteket. Vi kan se att den använder Javas "File" klass och anropar dess "exists()" metod för att kontrollera om mappen finns eller inte.

En viktig sak att notera är att "exists?"-funktionen inte kontrollerar om mappen är en giltig mapp, utan enbart om en fil eller mapp med det angivna namnet finns. Det är därför möjligt att funktionen returnerar "true" även om det inte är en giltig mapp eller om användaren inte har åtkomst till mappen.

## Se även

- [Dokumentation för Clojure biblioteket "clojure.java.io"](https://clojure.github.io/clojure/clojure.java.io-api.html)
- [Java File klassens dokumentation för "exists()" metoden](https://docs.oracle.com/javase/8/docs/api/java/io/File.html#exists--)