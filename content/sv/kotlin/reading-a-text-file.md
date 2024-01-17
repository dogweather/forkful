---
title:                "Läsa en textfil"
html_title:           "Kotlin: Läsa en textfil"
simple_title:         "Läsa en textfil"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Läsning av en textfil är en vanlig uppgift för programmerare, där man läser in innehållet av en textfil och använder detta i sin programkod. Det kan vara användbart för att hämta data från en annan källa, som en databas eller en extern fil.

## Så här gör du:
```
Kotlin
val file = File("textfil.txt")
file.forEachLine {
   println(it)
}
```
Detta kodexempel visar hur man kan öppna och läsa in innehållet av en textfil i Kotlin. ```val``` används för att skapa en variabel som kan referera till textfilen. Sedan används ```forEachLine``` för att iterera över varje rad i filen och skriva ut den på skärmen.

## Djupdykning:
Att läsa en textfil är en grundläggande uppgift, men det finns många olika sätt att göra det på. En alternativ metod är att använda en ```BufferedReader``` för att läsa en textfil rad för rad. Det är också viktigt att tänka på hur filen är kodad, till exempel om det är en textfil i UTF-8 eller ANSI-format.

## Se även:
Här är några användbara länkar för att lära dig mer om att läsa textfiler i Kotlin:
* [Kotlin File IO](https://kotlinlang.org/docs/tutorials/kotlin-for-py/file-io.html)
* [Java Tutorial: Reading a Text File](https://docs.oracle.com/javase/tutorial/essential/io/file.html)
* [Kotlin Text Processing](https://learnxinyminutes.com/docs/kotlin/)