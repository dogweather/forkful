---
title:                "Kotlin: Kontrollera om en mapp existerar"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför

Att kontrollera om en mapp (directory) existerar är en vanlig uppgift som många programmerare stöter på. Det kan till exempel vara användbart när man vill verifiera att en användare har tillgång till en viss mapp innan man utför en operation på den. I denna guide kommer vi att titta närmare på hur man på ett enkelt sätt kan kontrollera om en mapp finns i Kotlin.

## Så här gör du

För att kontrollera om en mapp existerar i Kotlin behöver vi importera paketet `java.io.File`. Sedan kan vi använda funktionen `exists()` som returnerar en `Boolean` som indikerar om mappen finns eller inte. Här nedan visas ett exempel på hur man kan implementera detta.

```Kotlin
import java.io.File

fun main() {
    val folder = File("path/to/folder")

    if(folder.exists()) {
        println("Mappen finns!")
    } else {
        println("Mappen finns inte.")
    } 
}
```

Om mappen `path/to/folder` existerar kommer "Mappen finns!" att skrivas ut, annars kommer "Mappen finns inte." att visas. Vi kan också använda `exists()` tillsammans med `if-else` för att utföra olika operationer beroende på om mappen finns eller inte.

## Djupdykning

Om vi vill ska vi kontrollera om mappen faktiskt är en mapp och inte en fil, så kan vi använda oss av funktionen `isDirectory()`. Den här funktionen returnerar också en `Boolean`, men den kommer att returnera `false` om mappen inte är en mapp och istället vara en fil eller inte finns alls. Detta kan vara användbart om man vill hantera olika typer av filer på olika sätt.

```Kotlin
fun main() {
    val folder = File("path/to/folder")

    if(folder.exists() && folder.isDirectory()) {
        println("Mappen finns och är en mapp!")
    } else {
        println("Mappen finns inte, eller så är det inte en mapp.")
    } 
}
```

## Se även

* [Kotlin - Working with Files](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/)
* [Java - File class](https://docs.oracle.com/javase/7/docs/api/java/io/File.html)