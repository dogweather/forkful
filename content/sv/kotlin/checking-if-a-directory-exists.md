---
title:    "Kotlin: Kontrollera om en mapp finns"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Varför

Att kolla om en mapp finns är en vanlig och användbar uppgift inom programmering. Det kan vara användbart när du behöver kontrollera om en specifik mapp existerar innan du skapar en ny fil eller utför andra åtgärder på den. Det hjälper också till att undvika felmeddelanden och undvika att skriva över befintliga filer.

## Hur man gör det

För att kolla om en mapp finns i Kotlin, kan du använda funktionen `exists()` från klassen `File`. Här är ett exempel som visar hur du kan göra det:

```Kotlin
val directory = File("path/to/directory")

if (directory.exists()) {
    println("Mappen finns!")
} else {
    println("Mappen finns inte.")
}
```

Om mappen finns, kommer programmet att skriva ut "Mappen finns!", annars kommer det att skriva ut "Mappen finns inte.".

## Deep Dive

I bakgrunden använder `exists()` funktionen `File#exists()` som returnerar `true` om filen eller mappen existerar. Detta fungerar genom att använda operativsystemets filsystem för att söka efter filen eller mappen. Om filen inte hittas kommer `File#exists()` att returnera `false`.

Notera att `exists()` funktionen inte garanterar att filen kommer att finnas kvar när försök görs att utföra åtgärder på den. Det är fortfarande möjligt för filen att raderas eller flyttas under tiden programmet körs.

## Se också

- [Kotlin File API](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/index.html)
- [Java File API](https://docs.oracle.com/javase/8/docs/api/java/io/File.html#exists--)