---
title:                "Een tijdelijk bestand aanmaken"
date:                  2024-01-28T21:58:18.851594-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een tijdelijk bestand aanmaken"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/java/creating-a-temporary-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Het maken van een tijdelijk bestand betekent het creëren van een bestand dat slechts voor korte tijd nodig is en daarna wordt verwijderd. Programmeurs doen dit voor tijdelijke opslag, zoals wanneer je gegevens moet bewaren tussen stappen in een proces of gevoelige informatie uit de langetermijnopslag moet houden.

## Hoe:

In Java is het `java.nio.file` pakket je vriend voor tijdelijke bestanden. Bekijk dit fragment:

```java
import java.nio.file.Files;
import java.nio.file.Path;

public class TempFileExample {
    public static void main(String[] args) {
        try {
            // Een tijdelijk bestand maken
            Path tempFile = Files.createTempFile(null, ".tmp");
            System.out.println("Tijdelijk bestand gecreëerd op: " + tempFile);

            // Schrijven naar tijdelijk bestand
            Files.writeString(tempFile, "Dit is de inhoud van een tijdelijk bestand");

            // Lezen van tijdelijk bestand
            String inhoud = Files.readString(tempFile);
            System.out.println("Inhoud van het tijdelijke bestand: " + inhoud);

            // Tijdelijk bestand verwijderen (optioneel hier omdat het bij JVM-afsluiting wordt verwijderd)
            Files.delete(tempFile);
            System.out.println("Tijdelijk bestand verwijderd.");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

Voer het uit, en je krijgt zoiets als:

```
Tijdelijk bestand gecreëerd op: /tmp/user23423842348234823948.tmp
Inhoud van het tijdelijke bestand: Dit is de inhoud van een tijdelijk bestand
Tijdelijk bestand verwijderd.
```

Netjes, toch?

## Diepgaande Bespreking

Tijdelijke bestanden maken al eeuwen deel uit van onze toolkit, helemaal terug tot de dageraad van het computergebruik. Ze zijn je beste keuze als je gegevens moet afhandelen die geen zaken hebben om te blijven hangen.

Java staat aan je zijde met de `Files` klasse sinds Java 7, wat de afhandeling van tijdelijke bestanden super eenvoudig maakt. Voordien moest je `File` objecten jongleren en hopen op het beste (maar ga niet terug naar die donkere dagen, omarm de nieuwe API).

Het coole aan de `createTempFile` methode is dat je de map en een bestandsnaam voorvoegsel of achtervoegsel kunt specificeren, of het allemaal aan Java's standaardwensen kunt overlaten. Onthoud gewoon, als je deze bestanden niet handmatig verwijdert, zullen ze blijven bestaan tot het programma wordt afgesloten. En in sommige gevallen, vooral bij langlopende toepassingen, wil je mogelijk zelf opruimen in plaats van te wachten op het grote finale.

Alternatieven? Zeker, je zou op de ouderwetse manier elke bestandsbewerking handmatig kunnen afhandelen, of een specifieke methode van het besturingssysteem gebruiken. Echter, de Java-weg is veiliger en meer overdraagbaar over platforms.

## Zie Ook

- [Java Path Klasse Documentatie](https://docs.oracle.com/javase/10/docs/api/java/nio/file/Path.html)
- [Java Files Klasse Documentatie](https://docs.oracle.com/javase/10/docs/api/java/nio/file/Files.html)
- [Oracle's Tutorial over Bestand I/O](https://docs.oracle.com/javase/tutorial/essential/io/file.html)