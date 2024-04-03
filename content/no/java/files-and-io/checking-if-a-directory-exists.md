---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:14.767147-07:00
description: "Hvordan: I Java er det flere m\xE5ter \xE5 sjekke om en mappe eksisterer,\
  \ prim\xE6rt ved bruk av `java.nio.file.Files` og `java.io.File` klassene. **Bruk\
  \ av\u2026"
lastmod: '2024-03-13T22:44:40.680641-06:00'
model: gpt-4-0125-preview
summary: "I Java er det flere m\xE5ter \xE5 sjekke om en mappe eksisterer, prim\xE6\
  rt ved bruk av `java.nio.file.Files` og `java.io.File` klassene."
title: Sjekker om en mappe eksisterer
weight: 20
---

## Hvordan:
I Java er det flere måter å sjekke om en mappe eksisterer, primært ved bruk av `java.nio.file.Files` og `java.io.File` klassene.

**Bruk av `java.nio.file.Files`**:

Dette er den anbefalte tilnærmingen i nyere Java-versjoner.

```java
import java.nio.file.Files;
import java.nio.file.Paths;

public class DirectoryExists {
    public static void main(String[] args) {
        // Angi mappens bane her
        String directoryPath = "path/to/directory";

        // Sjekker om mappen eksisterer
        if (Files.exists(Paths.get(directoryPath))) {
            System.out.println("Mappen eksisterer.");
        } else {
            System.out.println("Mappen eksisterer ikke.");
        }
    }
}
```
**Eksempel på Utdata**:
```
Mappen eksisterer.
```
Eller
```
Mappen eksisterer ikke.
```

**Bruk av `java.io.File`**:

Selv om `java.nio.file.Files` er anbefalt, kan også den eldre `java.io.File` klassen brukes.

```java
import java.io.File;

public class DirectoryExistsLegacy {
    public static void main(String[] args) {
        // Angi mappens bane her
        String directoryPath = "path/to/directory";

        // Oppretter et File objekt
        File directory = new File(directoryPath);

        // Sjekker om mappen eksisterer
        if (directory.exists() && directory.isDirectory()) {
            System.out.println("Mappen eksisterer.");
        } else {
            System.out.println("Mappen eksisterer ikke.");
        }
    }
}
```
**Eksempel på Utdata**:
```
Mappen eksisterer.
```
Eller
```
Mappen eksisterer ikke.
```

**Bruk av Tredjepartsbiblioteker**:

Selv om det vanlige Java-biblioteket vanligvis er tilstrekkelig for denne oppgaven, tilbyr tredjepartsbiblioteker som Apache Commons IO ytterligere filbehandlingsverktøy som kan være nyttige i mer komplekse applikasjoner.

**Apache Commons IO**:

Først, legg til Apache Commons IO-avhengigheten til prosjektet ditt. Deretter kan du bruke funksjonene dens til å sjekke om en mappe eksisterer.

```java
// Antatt at Apache Commons IO er lagt til prosjektet

import org.apache.commons.io.FileUtils;

public class DirectoryExistsCommons {
    public static void main(String[] args) {
        // Angi mappens bane her
        String directoryPath = "path/to/directory";

        // Bruker FileUtils til å sjekke
        boolean directoryExists = FileUtils.directoryContains(new File(directoryPath), null);

        if (directoryExists) {
            System.out.println("Mappen eksisterer.");
        } else {
            System.out.println("Mappen eksisterer ikke.");
        }
    }
}
```

**Merk**: `FileUtils.directoryContains` sjekker om en mappe inneholder en spesifikk fil, men ved å sende `null` som det andre argumentet, kan du bruke den til å sjekke for mappens eksistens. Vær forsiktig, ettersom dette kanskje ikke er den mest rettfram eller tiltenkte bruken av metoden.

**Eksempel på Utdata**:
```
Mappen eksisterer.
```
Eller
```
Mappen eksisterer ikke.
```
