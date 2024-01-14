---
title:    "Clojure: Sammanfogning av strängar"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

#Varför
Att konkatenera strängar är en viktig del av programmering eftersom det möjliggör att skapa mer dynamiska och anpassningsbara texter och meddelanden i dina program. Genom att kombinera flera strängar kan du skapa mer komplexa, men ändå lättlästa, utdata.

##Hur man gör det

För att konkatenera strängar i Clojure använder du funktionen `str`. Detta gör att du kan kombinera flera strängar till en enda sträng.

```Clojure
(str "Hej " "världen!") ; Output: "Hej världen!"
```

Du kan också använda `str` för att konvertera andra datatyper till strängar och sedan konkatenera dem.

```Clojure
(str 10 "äpplen") ; Output: "10 äpplen"
```

En annan användbar funktion för att konkatenera strängar är `format`. Detta gör det möjligt att skapa en mall med platshållare för variabler som sedan kan fyllas i när du konverterar till en sträng.

```Clojure
(format "Jag har %d äpplen och %d bananer." 5 3) ; Output: "Jag har 5 äpplen och 3 bananer."
```

##Djupdykning

I vissa fall kan konkatenering av strängar vara ineffektivt eftersom varje gång en sträng konkateneras skapas en helt ny sträng. Detta kan leda till onödig minnesanvändning och långsammare prestanda. För att undvika detta kan du istället använda en serie av strängar som läggs till en vector och sedan slå ihop dem med `apply` funktionen.

```Clojure
(apply str ["Hej " "världen!"]) ; Output: "Hej världen!"
```

En annan viktig sak att tänka på är skillnaden mellan `str` och `format`. `str` är snabbare eftersom det inte behöver tolka en mall, men `format` ger mer kontroll över formateringen av din utdata.

##Se även
- [Clojure Dokumentation - Strings](https://clojure.org/reference/strings)
- [Clojure Cookbook - Concatenating Strings](https://clojure-cookbook.com/strings/concatenating_strings)
- [Clojure for the Brave and True - Strings](https://www.braveclojure.com/strings/)