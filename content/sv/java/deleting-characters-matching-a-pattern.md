---
title:                "Radera tecken som matchar ett mönster."
html_title:           "Java: Radera tecken som matchar ett mönster."
simple_title:         "Radera tecken som matchar ett mönster."
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att ta bort tecken som matchar ett mönster är en vanlig programmeringsuppgift där man tar bort vissa tecken från en sträng eller textfil baserat på ett angivet mönster. Detta kan användas för att rensa oönskade tecken eller för att göra en sträng mer läsbar och formaterad.

## Hur man gör:
```Java
String text = "Den här strängen är programmerad för att ta bort alla siffror.";
String newText = text.replaceAll("\\d", ""); 
System.out.println(newText);
```
Output: "Den här strängen är programmerad för att ta bort alla siffror."

```Java
String text = "Java är bäst <3";
String newText = text.replaceAll("Java", "Python"); 
System.out.println(newText);
```
Output: "Python är bäst <3"

## Djupdykning:
(1) Att matcha och ta bort tecken från en sträng har funnits sedan de tidiga dagarna av datorprogrammering. (2) Det finns också andra sätt att göra samma sak, som att använda en for-loop och jämföra varje tecken i strängen med det angivna mönstret. (3) I Java används metoden `replaceAll()` för att ersätta alla instanser av ett mönster i en sträng med en annan text.

## Se även:
- [Java String Class](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Java Regular Expressions](https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html)