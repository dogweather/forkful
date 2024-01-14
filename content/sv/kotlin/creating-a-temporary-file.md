---
title:    "Kotlin: Skapa en temporär fil"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Varför
Att skapa en tillfällig fil kan vara användbart när man tillfälligt behöver lagra data eller skapa en temporär plats för att utföra en viss uppgift. Det är också ett sätt att hålla sin kod ren och organisera temporära filer på ett effektivt sätt.

## Hur
För att skapa en temporär fil i Kotlin kan man använda sig av klassen `java.io.File` och metoderna `createTempFile()` för att skapa en ny fil och `deleteOnExit()` för att radera filen när programmet avslutas. Se nedan för ett exempel:

```Kotlin
val tempFile = File.createTempFile("temp", ".txt")
tempFile.deleteOnExit()
println("Temporär fil skapad med namn " + tempFile.name)
```
Output:
```
Temporär fil skapad med namn temp2360808707802368133.txt
```
I exemplet ovan skapas en temporär fil med prefixet "temp" och suffixet ".txt". När programmet stängs kommer filen att raderas automatiskt.

## Djupdykning
Förutom att skapa temporära filer med `createTempFile()` finns det också möjlighet att skapa temporära kataloger med metoden `createTempDirectory()`. Det finns också olika sätt att hantera filens livscykel, såsom att ändra dess behörigheter och skapa en temporär fil med ett specifikt innehåll.

För en mer detaljerad förklaring och fler exempel, se Kotlin dokumentationen för `java.io.File` eller andra resurser på nätet.

## Se även
- [Kotlin dokumentation - java.io.File](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/index.html)
- [Översikt av Kotlin filhantering](https://www.programiz.com/kotlin-programming/files)
- [Skapa och hantera temporära filer i Kotlin](https://www.baeldung.com/kotlin-temporary-file)