---
title:                "Skapa en tillfällig fil"
html_title:           "Kotlin: Skapa en tillfällig fil"
simple_title:         "Skapa en tillfällig fil"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Varför

Att skapa temporära filer kan vara användbart i många olika scenarion. Det kan hjälpa till att hantera data som behövs för en kort tid, eller för att undvika namnkollisioner i filnamn.

## Hur man gör

För att skapa en temporär fil i Kotlin, använd `File.createTempFile()` funktionen. Detta kommer att skapa en temporär fil i standardplatsen för temporära filer på systemet.

```Kotlin
val tempFile = File.createTempFile("temp", ".txt")
println(tempFile.absolutePath)

// Output:
// C:\Users\User\AppData\Local\Temp\temp4515947704231343704.txt
```

Det första argumentet i `createTempFile()` funktionen är prefixet som används i filnamnet, följt av prefix separatorsymbolen och suffixet för filen. I vårt exempel har vi använt "temp" som prefix och ".txt" som suffix, vilket ger oss ett filnamn som "temp4515947704231343704.txt".

Om vi vill ange en annan plats för den temporära filen kan vi lägga till en `directory` parameter i funktionen:

```Kotlin
val tempFile = File.createTempFile("temp", ".txt", File("C:/temp/"))
println(tempFile.absolutePath)

// Output:
// C:\temp\temp2543397389070155423.txt
```

För att ta bort en temporär fil när vi är klara med den, kan vi använda `delete()` funktionen:

```Kotlin
val tempFile = File.createTempFile("temp", ".txt", File("C:/temp/"))
println(tempFile.absolutePath)

tempFile.delete()

// Output:
// C:\temp\temp2543397389070155423.txt
// (Filen tas bort)
```

## Djupdykning

Standardplatsen för temporära filer på systemet varierar beroende på vilket operativsystem som används. I Windows är det vanligtvis "C:\Users\<username>\AppData\Local\Temp\", medan det i Linux är "/tmp/". Detta kan dock skilja sig åt beroende på systemkonfigurationen.

Det är också möjligt att skapa en temporär fil med en specifik prefix och suffix i en vald mapp genom att använda `File.createTempFile(prefix, suffix, directory)` funktionen.

## Se även

- [Kotlin's File API](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/)
- [Java's createTempFile() method](https://docs.oracle.com/javase/8/docs/api/java/io/File.html#createTempFile-java.lang.String-java.lang.String-java.io.File-)