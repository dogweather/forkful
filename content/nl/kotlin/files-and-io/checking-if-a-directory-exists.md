---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:04.853357-07:00
description: "Controleren of een map bestaat in Kotlin stelt je in staat te bevestigen\
  \ of een map aanwezig is voordat je probeert er bestanden te lezen of te schrijven.\u2026"
lastmod: '2024-03-11T00:14:24.606132-06:00'
model: gpt-4-0125-preview
summary: "Controleren of een map bestaat in Kotlin stelt je in staat te bevestigen\
  \ of een map aanwezig is voordat je probeert er bestanden te lezen of te schrijven.\u2026"
title: Controleren of een directory bestaat
---

{{< edit_this_page >}}

## Wat & Waarom?

Controleren of een map bestaat in Kotlin stelt je in staat te bevestigen of een map aanwezig is voordat je probeert er bestanden te lezen of te schrijven. Dit helpt fouten zoals `FileNotFoundException` te voorkomen en helpt je programma slimme beslissingen te nemen, zoals het maken van de map als deze ontbreekt.

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
