---
title:    "Kotlin: Läsning av en textfil"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Varför läsa en textfil?

Att läsa en textfil är en grundläggande uppgift inom programmering och kan vara användbart för att hämta data från en extern källa, såsom en databas eller internet. Det är också ett bra sätt att lära sig hur man hanterar olika filformat och datatyper.

# Så här gör du det:

```Kotlin
val fil = File("min_textfil.txt")
val radListe = fil.readLines()

for (rad in radListe) {
    println(rad)
}
```
Det första steget är att skapa en instans av File-klassen och peka på den textfil som du vill läsa. Sedan använder vi metoden "readLines()" för att läsa in texten i filen och lagra den i en lista. Sedan kan vi använda en for-loop för att skriva ut varje rad i filen.

Output:
```
Det här är en textfil.

Här är lite mer text.

Slutligen, lite till text.
```

# Djupdykning:

Det finns flera olika sätt att läsa en textfil på, beroende på hur du vill hantera och använda datat som finns i filen. Förutom att använda "readLines()", finns det också metoder som "readText()" och "readBytes()" som kan användas för att läsa filen som en sträng eller en array av bytes.

En annan viktig aspekt att tänka på är hur du hanterar eventuella fel som kan uppstå. Det är viktigt att inkludera en try-catch-block för att fånga eventuella IOExceptions som kan uppstå när filen läses.

## Se även:

- [Kotlin's Dokumentation om File-läsning](https://kotlinlang.org/docs/tutorials/kotlin-for-py/file-input-and-output.html)
- [Java's Dokumentation om filhantering](https://docs.oracle.com/javase/tutorial/essential/io/file.html)
- [GeeksforGeeks's Guide till att läsa en textfil i Kotlin](https://www.geeksforgeeks.org/kotlin-reading-file-operations/)