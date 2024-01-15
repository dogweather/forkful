---
title:                "Att skriva en textfil"
html_title:           "Kotlin: Att skriva en textfil"
simple_title:         "Att skriva en textfil"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Varför

Att skriva en textfil med Kotlin är ett praktiskt sätt att lagra data på ett enkelt och strukturerat sätt. Det är användbart för att spara information som ska användas senare, eller för att skapa enkel textbaserad output.

## Så här gör du

För att skriva en textfil i Kotlin följer du dessa steg:

1. Importera klassen `java.io.File`, som kommer att användas för att skapa en ny fil.
2. Skapa en instans av `File` klassen och ge filen ett namn.
3. Öppna filen för skrivning.
4. Använd `println()`-funktionen för att skriva dina önskade rader text till filen.
5. Stäng filen när du är klar genom att anropa `close()`-metoden på filobjektet.

Här är ett exempel på kod som skapar en textfil med namnet "mitt_textdokument.txt" och skriver två rader text till filen:

```Kotlin
import java.io.File

val mittFilobjekt = File("mitt_textdokument.txt")
mittFilobjekt.createNewFile()              // Skapar filen om den inte redan finns

val text = "Detta är den första raden.\nDetta är den andra raden."
mittFilobjekt.printWriter().use { out ->  // Öppnar filen för skrivning och använder printWriter
   for (rad in text.lines()) {             // Loopar genom varje rad i text
      out.println(rad)                     // Skriver raden till filen
   }
}
```

När koden körs så kommer en ny fil att skapas med de två raderna text som vi skrivit i vårt kodexempel ovan.

```
Detta är den första raden.
Detta är den andra raden.
```

## Djupdykning

I exemplet ovan använde vi `File` klassen för att skapa en ny textfil. I `println()` funktionen använde vi specialtecknet `\n` för att skapa en ny rad i vår textfil. Detta är känt som en "escape sequence" och finns i flera programmeringsspråk för att skapa speciella tecken som inte kan skrivas direkt i en sträng.

Vi kan också använda `File` klassen för att läsa från en befintlig textfil. Istället för att använda `printWriter()` funktionen, använder vi `bufferedReader()` för att öppna filen för läsning och läsa varje rad med `readLine()` funktionen. Här är ett exempel på hur man läser från en textfil:

```Kotlin
val filAttLasa = File("exempel.txt")
val buffert = filAttLasa.bufferedReader()       // Öppnar filen för läsning
var radLäst = buffert.readLine()                // Returnerar första raden i filen

while (radLäst != null) {                       // Loopar tills alla rader är lästa
    println(radLäst)                            // Skriver ut raden som lästs
    radLäst = buffert.readLine()                // Läser nästa rad
}
```

I detta exempel använder vi en while-loop för att läsa varje rad i filen tills `readLine()` returnerar null, vilket betyder att slutet av filen har nåtts.

## Se även

* [Kotlin officiell hemsida] (https://kotlinlang.org/) - Officiell hemsida för Kotlin som innehåller dokumentation och andra resurser.
* [Java IO-tutorial] (https://www.tutorialspoint.com/java/java_files_io.htm) - En detaljerad tutorial om inmatning och utmatning av filer i Java, som också gäller för Kotlin.
* [Escapesequences i Kotlin] (https://kotlinlang.org/docs/reference/basic-types.html#string-literals) - Dokumentation om kännande av escape sequences i Kotlin som används i textsträngar.