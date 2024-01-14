---
title:    "Kotlin: Skapa en tillfällig fil"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Varför

I Kotlin finns det en enkel och effektiv funktion för att skapa temporära filer. Detta kan vara användbart när du behöver skapa en fil temporärt för att sedan använda den i din kod eller radera den efteråt.

## Hur man gör

Skapandet av en temporär fil i Kotlin är enkelt. Du kan använda funktionen ```Kotlin File.createTempFile()``` för att skapa en temporär fil i valfri mapp. Till exempel:

```
val tempFile = File.createTempFile("temp", ".txt")
```

Detta kommer att skapa en temporär fil med namnet "temp" och slutändelsen ".txt" i det tillfälliga filsystemet på din dator. Om du vill kan du också specificera en annan mapp som första parameter, där den temporära filen ska skapas. Sample output kan se ut så här:

```
C:\Users\User\AppData\Local\Temp\temp253208952490630135.txt
```

Du kan även använda funktionen ```Kotlin File.deleteOnExit()``` för att ange att den temporära filen ska raderas automatiskt när ditt program avslutas. Detta är särskilt användbart om du bara vill ha en temporär fil som används under körningen av ditt program.

## Djupdykning

För att förstå hur temporära filer fungerar i Kotlin är det viktigt att veta att de skapas i det temporära filsystemet på din dator. Detta är en mapp som används för att temporärt lagra filer och data som inte behöver sparas permanent.

En av fördelarna med att använda sig av temporära filer är att du inte behöver tänka på att radera dem själv. Du kan också använda dem för att skapa unika filer för varje körning av ditt program, vilket kan vara användbart för att undvika problem med kollision av filnamn.

## Se även

- [Kotlin dokumentation om temporära filer](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/create-temp-file.html)
- [Artikel om temporära filer i Java](https://www.baeldung.com/java-temporary-file)