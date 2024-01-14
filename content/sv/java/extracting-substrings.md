---
title:                "Java: Extrahering av substrängar"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför

Att kunna extrahera substrängar, det vill säga en del av en större textsträng, är en viktig färdighet för alla Java-programmerare. Genom att kunna välja och manipulera specifika delar av en textsträng ger det oss möjlighet att hantera och behandla data mer effektivt.

## Hur man gör det

För att extrahera en substräng i Java använder man sig av metoden `substring()`. Denna metod tar två parametrar – startindex och slutindex – och returnerar en del av den ursprungliga strängen baserat på dessa parametrar.

Här är ett exempel på hur man kan använda `substring()`:

```java
String text = "Hej alla svenska läsare!";
String substr = text.substring(4, 14);

System.out.println(substr);
```

Detta kommer att skriva ut "alla svenska" eftersom substrängen börjar vid index 4 och slutar vid index 14 (inklusive index 4 men exklusive index 14).

Man kan också använda metoden `length()` för att hämta längden på en textsträng, vilket kan vara användbart för att bestämma slutindexet. Tänk på att index i Java alltid börjar vid 0, så för att extrahera den sista delen av en textsträng kan man använda `length()`-metoden för att hämta den sista indexpositionen.

```java
String text = "Hej alla svenska läsare!";
String lastPart = text.substring(18, text.length());

System.out.println(lastPart);
```

Detta kommer att skriva ut "läsare!" eftersom den sista indexpositionen av `text` är 23 och vi utgår från 18 för att extrahera den sista delen av strängen.

## Djupdykning

Det är viktigt att vara medveten om att Java behandlar textsträngar och index på ett annat sätt än vissa andra programmeringsspråk, såsom C++. I Java är textsträngar objekt, vilket betyder att de har metoder som kan anropas på dem, till exempel `substring()`.

När det gäller index räknar Java alla tecken, inklusive mellanslag, i en textsträng. Detta är viktigt att komma ihåg eftersom det kan påverka vilka index som man behöver använda för att extrahera den önskade substängen.

En annan viktig detalj är att `substring()`-metoden returnerar en ny sträng och påverkar inte den ursprungliga strängen. Detta gör det möjligt för oss att använda `substring()` flera gånger på samma sträng och få olika substrängar som resultat.

## Se också

- [Java officiell dokumentation för `substring()`](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#substring-int-int-)
- [Java video tutorial om `substring()`](https://www.youtube.com/watch?v=0HwQrf_JP6o)
- [Mer information om index och textsträngar i Java](https://www.geeksforgeeks.org/java-string-substring-method/)