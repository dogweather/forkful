---
title:                "Extrahera delsträngar"
html_title:           "Kotlin: Extrahera delsträngar"
simple_title:         "Extrahera delsträngar"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## Varför

Om du någonsin behövt extrahera en del av en textsträng i ditt Kotlin-program, har du säkert undrat hur du ska göra det på ett enkelt och effektivt sätt. Att lära sig hur man extraherar substrängar kan hjälpa dig att hantera och manipulera text på ett smidigare sätt och göra ditt kodande liv lite enklare.

## Så här gör du

Extrahera en substräng i Kotlin är en relativt enkel process. Här är ett exempel på hur du kan göra det:

```Kotlin
val text = "Det här är ett exempel"
val substräng = text.subSequence(0, 9)
println(substräng)
```
Output:
```
Det här är 
```
För att extrahera en del av en textsträng använder vi funktionen "subSequence()" tillsammans med index för början och slut på den del av strängen som du vill extrahera. I kodexemplet ovan använder vi index 0 och 9 för att få en substräng av de första 9 tecknen i den ursprungliga strängen. 

Du kan även använda funktionen "substring()" om du vill extrahera en del av en textsträng baserat på tecken istället för index. Här är ett annat exempel:

```Kotlin
val text = "Det här är ett exempel"
val substräng = text.substring(0, 9)
println(substräng)
```
Output:
```
Det här är 
```
Det finns även möjligheten att använda regex för att extrahera en del av en textsträng som matchar ett visst mönster. 

## Djupdykning

Det finns vissa saker du bör hålla i åtanke när du extraherar substrängar i Kotlin. Först och främst, om indexet för slutet är längre än längden på den ursprungliga strängen, kommer funktionen att kasta ett "IndexOutOfBoundsException". Detta kan undvikas genom att kontrollera längden på strängen innan du utför subSträng- operationen.

Det är också viktigt att komma ihåg att strängar är oföränderliga i Kotlin, vilket betyder att en substräng är en ny sträng och ändringar i en substräng kommer inte att påverka den ursprungliga strängen. 

## Se även

- [Kotlin Standardbibliotek](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string/#subSequence)
- [Java String API](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#substring(int, int))
- [Kotlin Regex](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)