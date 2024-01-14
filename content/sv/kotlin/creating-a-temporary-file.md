---
title:                "Kotlin: Skapa en temporär fil"
simple_title:         "Skapa en temporär fil"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Varför Skulle Du Vilja Skapa En Tillfällig Fil
Att skapa en tillfällig fil kan vara användbart i många olika situationer. Till exempel om du behöver lagra tillfällig data under körning av ditt program eller när du vill testa någon funktionalitet utan att behöva permanent lagra data på din enhet. Det kan också vara en smart lösning när du arbetar med filhantering och behöver skapa tillfälliga backup-filer.

## Hur Du Skapar En Tillfällig Fil i Kotlin
Kotlin har inbyggda funktioner för att skapa och hantera tillfälliga filer på ett smidigt sätt. Det finns flera olika sätt att göra detta på, men vi kommer att fokusera på användning av `createTempFile()`-funktionen.

```Kotlin
val tempFile = createTempFile("prefix", "suffix")
println(tempFile.absolutePath)
```

Koden ovan skapar en tillfällig fil med namnet "prefix" plus en genererad unik identifierare och med filändelsen "suffix". Det är viktigt att notera att filen automatiskt kommer att raderas när ditt program avslutas.

## Djupgående Information om Skapande av Tillfälliga Filer
Det finns flera olika parametrar som kan läggas till i `createTempFile()`-funktionen för att förbättra hanteringen av din tillfälliga fil. Till exempel kan du ange en specifik mapplats där filen ska skapas, vilket kan vara användbart om du vill hålla koll på var dina tillfälliga filer lagras. Du kan också ange ett prefix för filnamnet istället för att använda det genererade prefixet som standard, och du kan till och med ange en `deleteOnExit`-parameter för att förhindra att filen raderas vid avslutning av ditt program.

```Kotlin
val tempFile = createTempFile("prefix", "suffix", File("/Users/username/Desktop"))
println(tempFile.absolutePath)
```

Som nämnts tidigare kommer tillfälliga filer automatiskt att raderas när ditt program avslutas. Om du däremot vill ta bort filen manuellt kan du göra det med hjälp av `delete()`-funktionen.

```Kotlin
val tempFile = createTempFile("prefix", "suffix")
tempFile.delete()
```

# Se Även
- [Kotlin documentation: Skapa tillfällig fil](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/create-temp-file.html)
- [Java Tutorials: Skapa en tillfällig fil](https://docs.oracle.com/javase/tutorial/essential/io/file.html#temporary) 
- [Baeldung: Arbeta med tillfälliga filer i Java](https://www.baeldung.com/java-temporary-files)