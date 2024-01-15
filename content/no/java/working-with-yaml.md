---
title:                "Å jobbe med yaml"
html_title:           "Java: Å jobbe med yaml"
simple_title:         "Å jobbe med yaml"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/working-with-yaml.md"
---

{{< edit_this_page >}}

## Hvorfor 
Hvis du er en Java-utvikler og trenger å konfigurere applikasjoner eller håndtere datastrukturer, har du kanskje hørt om YAML. Dette er et enkelt og leselig filformat som kan være nyttig for å lagre og flytte data mellom forskjellige programmer.

## Slik bruker du YAML i Java
For å bruke YAML i Java, må du først inkludere en tredjepartsavhengighet i ditt prosjekt. Du kan gjøre dette ved å legge til følgende linje i pom.xml-filen din:

```Java
<dependency>
    <groupId>org.yaml</groupId>
    <artifactId>snakeyaml</artifactId>
    <version>1.24</version>
</dependency>
```

Etter å ha konfigurert avhengigheten, kan du nå begynne å bruke YAML i ditt Java-prosjekt. Først må du importere klassene for å lese og skrive YAML-filer:

```Java
import org.yaml.snakeyaml.Yaml;
import java.io.InputStream;
import java.io.OutputStream;
```

For å lese fra en YAML-fil, kan du bruke følgende kode:

```Java
// Åpne en InputStream for YAML-filen
InputStream input = new FileInputStream("minfil.yaml");
// Opprett et Yaml-objekt
Yaml yaml = new Yaml();
// Bruk load-metoden for å lese YAML-filen og lagre det i et Java-objekt
Object data = yaml.load(input);
// Skriv ut dataene som er lagret i objektet
System.out.println(data);
```

For å skrive til en YAML-fil, kan du bruke følgende kode:

```Java
// Åpne en OutputStream for å skrive til filen
OutputStream output = new FileOutputStream("minfil.yaml");
// Opprett et Yaml-objekt
Yaml yaml = new Yaml();
// Definer et Java-objekt som du vil lagre i YAML-filen
Map<String, String> map = new HashMap<>();
map.put("key1", "value1");
map.put("key2", "value2");
// Bruk dump-metoden for å skrive dataene til YAML-filen
yaml.dump(map, output);
```

Etter å ha lest eller skrevet til en YAML-fil, kan du også arbeide med dataene i et Java-objekt ved hjelp av standardmetoder.

## Dypdykk i YAML
YAML støtter forskjellige datatyper, inkludert strenger, tall, lister og kart. Det er også mulig å inkludere kommentarer i dine YAML-filer. Du kan utforske disse funksjonene mer detaljert ved å lese dokumentasjonen for SnakeYAML-biblioteket.

## Se også
- [SnakeYAML dokumentasjon](https://bitbucket.org/asomov/snakeyaml/wiki/Documentation)
- [YAML offisiell side](https://yaml.org/)
- [Java-YAML bibliotek](https://github.com/FasterXML/jackson-dataformats-text)