---
title:                "Skriva till standardfel"
date:                  2024-01-19
html_title:           "Arduino: Skriva till standardfel"
simple_title:         "Skriva till standardfel"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Skriva till standard error (stderr) betyder att rikta felmeddelanden och diagnostik till en separat ström. Programmerare gör detta för att skilja normala programutdata från fel och debugginformation.

## Hur gör man:
```Kotlin
fun main() {
    System.err.println("Ett fel inträffade!")
}
```
**Output:**
```
Ett fel inträffade!
```

## Fördjupning
Historiskt sett användes stderr i Unix's tidiga dagar för att separera normala data (stdout) från fel (stderr). Ett alternativ är att loggföra till en fil, men stderr är effektiv för realtidsfel och debug. Implementeringsmässigt är stderr en global `PrintStream`-instans som Kotlin ärver från Java.

## Se även
- [Kotlins officiella dokumentation](https://kotlinlang.org/docs/home.html)
- [Java PrintStream-klassen](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/io/PrintStream.html)
- [Unix's standard strömmar](https://en.wikipedia.org/wiki/Standard_streams)
