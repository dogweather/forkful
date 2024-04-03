---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:04.853357-07:00
description: 'Hoe: In Kotlin gebruik je doorgaans `java.io.File` (uit de standaardbibliotheek
  van Java) om te controleren op een map. Hier is een snel voorbeeld.'
lastmod: '2024-03-13T22:44:50.782331-06:00'
model: gpt-4-0125-preview
summary: In Kotlin gebruik je doorgaans `java.io.File` (uit de standaardbibliotheek
  van Java) om te controleren op een map.
title: Controleren of een directory bestaat
weight: 20
---

## Hoe:
In Kotlin gebruik je doorgaans `java.io.File` (uit de standaardbibliotheek van Java) om te controleren op een map. Hier is een snel voorbeeld:

```Kotlin
import java.io.File

fun main() {
    val directoryPath = "pad/naar/map"
    val directory = File(directoryPath)

    if (directory.exists() && directory.isDirectory) {
        println("De map bestaat!")
    } else {
        println("De map bestaat niet.")
    }
}
```

Voorbeelduitvoer wanneer de map bestaat:

```
De map bestaat!
```

En wanneer het niet zo is:

```
De map bestaat niet.
```

## Diepgaande Duik
De methode `exists()` in Java bestaat al sinds de vroege dagen van Java. Toen Kotlin kwam, behield het sterke interoperabiliteit met Java, waardoor het mogelijk werd direct Java-bibliotheken te gebruiken. `exists()` retourneert `true` als een bestand of map bestaat, maar om zeker te weten dat het een map is en geen bestand, controleren we ook `isDirectory`.

Nu, alternatieven:

- **Kotlin's java.nio.file pakket**: Het biedt `Files.exists(path)` en `Files.isDirectory(path)`. Deze methoden werken op een vergelijkbare manier maar bieden meer controle over bestandsattributen.

- **Kotlin-specifieke bibliotheken**: Sommige door de gemeenschap aangedreven bibliotheken breiden Kotlin's vermogen om met bestanden te werken uit. Ze bieden meer idiomatische Kotlin-oplossingen, maar onder de motorkap zijn het vaak simpelweg wrappers rond Java's I/O-klassen.

Werken met mappen is een mix van aanwezigheid controleren (bestaat het?) en type (is het een bestand of map?). Beide controles zijn essentieel om te voorkomen dat je programma struikelt over onverwachte bestandssysteemtoestanden.

## Zie Ook
- [`File` API-documentatie](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/io/File.html) - Duik in wat je kunt doen met `File`.
- [`Files` API-documentatie in java.nio.file pakket](https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html) - Voor geavanceerde bestandsbewerkingen.
- [Officiële documentatie van Kotlin](https://kotlinlang.org/docs/home.html) - Leer meer over de mogelijkheden van Kotlin.
- [Stack Overflow](https://stackoverflow.com/questions/tagged/kotlin) - Vind door de gemeenschap aangedragen problemen en oplossingen.
