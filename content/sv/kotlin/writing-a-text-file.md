---
title:                "Skriva en textfil"
date:                  2024-01-19
html_title:           "Arduino: Skriva en textfil"
simple_title:         "Skriva en textfil"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skriva till en textfil innebär att spara data som textsträngar i en fil. Programmerare gör det för att bevara data mellan sessioner, dela information eller för loggning.

## Hur gör man:
```Kotlin
import java.io.File

fun main() {
    val text = "Hej, det här är text sparad i en fil!"
    File("exempel.txt").writeText(text)
    println("Texten har sparats!")
}
```
Detta skapar en fil `exempel.txt` och skriver texten "Hej, det här är text sparad i en fil!" till den.

## Fördjupning
Att skriva textfiler är ett klassiskt programmeringsbehov som har funnits ända sedan de tidiga datordagarna. Alternativ till `writeText` inkluderar strömmar (som `FileOutputStream` eller `PrintWriter`) för större filer eller mer kontroll. `writeText` använder `UTF-8` som standard och skriver om filen; för att lägga till text, använd `appendText`.

## Se även
- [Kotlin API Reference for File](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/)
- [Tutorial on file handling in Kotlin](https://kotlinlang.org/docs/tutorials/kotlin-for-py/file-io.html)
