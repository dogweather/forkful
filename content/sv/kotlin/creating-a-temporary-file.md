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

## Vad & varför?
Skapandet av en tillfällig fil är en metod som programmerare använder för att skapa en temporär fil som endast behövs under en viss tid och sedan kan tas bort. Detta är användbart när man behöver lagra tillfälliga data eller när man vill undvika att skriva över eller förstöra en befintlig fil.

## Såhär gör du:
```Kotlin
// Skapa en temporär fil med en unik namngivning
val tempFile = File.createTempFile("myTempFile", ".txt") 

// Skriva till och läsa från filen
tempFile.writeText("Hello World!") 
val fileContent = tempFile.readText() 

// Ta bort filen när den inte behövs längre
tempFile.delete() 

// Skapa en temporär fil i en specifik mapp
val tempFile = File.createTempFile("myTempFile", ".txt", File("myTempFolder")) 
```

## Djupdykning:
Historisk kontext:
Skapandet av temporära filer har funnits länge inom programmering, men används fortfarande på grund av sin användbarhet och enkelhet.

Alternativ:
En annan metod för att skapa temporära filer är att använda sig av en temporär databas, men detta kan vara mer komplicerat och kräva mer resurser.

Implementeringsdetaljer:
Skapandet av en temporär fil innebär i huvudsak att en tom fil skapas på datorns hårddisk och tilldelas ett unikt namn. När filen tas bort av programmet så rensas även den temporära filen från hårddisken.

## Se även:
- [Kotlin Dokumentation: Skapa en temporär fil](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/create-temp-file.html) 
- [GeeksforGeeks: Creating Temporary Files in Java](https://www.geeksforgeeks.org/creating-temporary-files-java/)