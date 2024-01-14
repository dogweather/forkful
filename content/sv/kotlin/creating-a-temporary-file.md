---
title:                "Kotlin: Skapa en tillfällig fil"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Varför?

Att skapa en tillfällig fil kan vara en viktig del av många Kotlin-programmerares arbetsflöde. Det kan hjälpa till att hantera filer på ett effektivt sätt genom att skapa en temporär lagringsplats för data som behöver bearbetas, sorteras eller manipuleras innan de sparas permanent.

## Hur man gör det

Det finns ett enkelt sätt att skapa en tillfällig fil i Kotlin med hjälp av standardbiblioteket. Först måste vi importera "java.io" paketet och sedan använda "createTempFile()" metoden på den "File" klassen.

```Kotlin
import java.io.*

val tempFile = File.createTempFile("temp", ".txt")
println(tempFile.absolutePath)
```

I det här exemplet använder vi metoden för att skapa en temporär textfil med ett slumpmässigt genererat namn, och sedan skriver ut den fullständiga sökvägen till filen. Detta är användbart om vi vill manipulera filen eller spara den på en specifik plats.

### Utmatning:

`C:\Users\Namn\AppData\Local\Temp\temp3007607233760854608.txt`

## Fördjupning

När du skapar en tillfällig fil i Kotlin är det alltid bra att tänka på filens livscykel och när den ska tas bort. Det är viktigt att föra in en try-finally kod som tar bort den tillfälliga filen när det behövs.

```Kotlin
import java.io.*

val tempFile = File.createTempFile("temp", ".txt")
try {
    // gör något med den temporära filen
} finally {
    tempFile.delete()
}
```

Detta ser till att den tillfälliga filen tas bort även om det uppstår ett fel under bearbetningen.

## Se även

- Officiell dokumentation för "java.io.File" klassen: https://docs.oracle.com/javase/7/docs/api/java/io/File.html
- "createTempFile()" metoden för att skapa tillfälliga filer: https://docs.oracle.com/javase/7/docs/api/java/io/File.html#createTempFile%28java.lang.String,%20java.lang.String%29