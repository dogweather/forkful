---
title:                "Kontrollera om en katalog existerar"
html_title:           "Kotlin: Kontrollera om en katalog existerar"
simple_title:         "Kontrollera om en katalog existerar"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Vad & varför?
Att kontrollera om en mapp finns är ett sätt för programmerare att verifiera om en viss mapp finns i ett givet system eller inte. Det är ett viktigt steg när man skapar funktioner för att läsa, skriva eller navigera i olika mappar.

# Hur man gör:
Kotlin har en inbyggd funktion för att kontrollera om en mapp finns eller inte. Använd ```File(filePath).exists()``` för att kontrollera om en mapp, representerad av ```filePath```, finns. Om filen finns returnerar funktionen "true", annars returnerar den "false".

## Kodexempel:
```
val directory = File("C:/Users/Desktop") 
val exists = directory.exists() 

if (exists) {
  println("Mappen existerar!")
} else {
  println("Mappen finns inte.")
}

// Output: Mappen existerar!
```

## Djupdykning:
Att kontrollera om en mapp finns har varit en viktig del av programmering sedan den första datorn skapades. Idag har det blivit ännu viktigare när vi hanterar stora mängder information i våra system och behöver veta exakt var alla filer och mappar finns.

Alternativet till att använda ```exists()```-funktionen är att använda ```File.isFile()``` eller ```File.isDirectory()``` för att kontrollera om en fil eller en mapp finns. Dessa funktioner returnerar också "true" eller "false" beroende på resultatet.

## Se även:
- [Dokumentation för filsystemet i Kotlin](https://kotlinlang.org/docs/reference/files.html)
- [Mer om hantering av filer i Kotlin](https://www.geeksforgeeks.org/file-handling-in-kotlin-reading-from-a-text-file/)
- [Kotlin Cookbook](https://www.oreilly.com/library/view/kotlin-cookbook/9781788472145/) av Ken Kousen