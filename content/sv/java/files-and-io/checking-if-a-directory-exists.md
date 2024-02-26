---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:58.608216-07:00
description: "Att kontrollera om en katalog finns i Java \xE4r en grundl\xE4ggande\
  \ uppgift som inneb\xE4r att man verifierar n\xE4rvaron av en katalog i filsystemet\
  \ innan man l\xE4ser\u2026"
lastmod: '2024-02-25T18:49:36.096578-07:00'
model: gpt-4-0125-preview
summary: "Att kontrollera om en katalog finns i Java \xE4r en grundl\xE4ggande uppgift\
  \ som inneb\xE4r att man verifierar n\xE4rvaron av en katalog i filsystemet innan\
  \ man l\xE4ser\u2026"
title: Kontrollera om en katalog existerar
---

{{< edit_this_page >}}

## Vad & Varför?
Att kontrollera om en katalog finns i Java är en grundläggande uppgift som innebär att man verifierar närvaron av en katalog i filsystemet innan man läser från den, skriver till den eller utför andra operationer som kräver dess existens. Detta är avgörande för att undvika fel eller undantag i program som interagerar med filsystemet, vilket säkerställer en smidigare exekvering och bättre användarupplevelse.

## Hur man gör:
I Java finns det flera sätt att kontrollera om en katalog finns, främst genom att använda klasserna `java.nio.file.Files` och `java.io.File`.

**Använda `java.nio.file.Files`:**

Det här är den rekommenderade metoden i senare Java-versioner.

```java
import java.nio.file.Files;
import java.nio.file.Paths;

public class DirectoryExists {
    public static void main(String[] args) {
        // Ange sökvägen till katalogen här
        String directoryPath = "path/to/directory";

        // Kontrollera om katalogen finns
        if (Files.exists(Paths.get(directoryPath))) {
            System.out.println("Katalogen finns.");
        } else {
            System.out.println("Katalogen finns inte.");
        }
    }
}
```
**Exempel på utmatning**:
```
Katalogen finns.
```
Eller
```
Katalogen finns inte.
```

**Använda `java.io.File`:**

Även om `java.nio.file.Files` rekommenderas, kan den äldre klassen `java.io.File` också användas.

```java
import java.io.File;

public class DirectoryExistsLegacy {
    public static void main(String[] args) {
        // Ange sökvägen till katalogen här
        String directoryPath = "path/to/directory";

        // Skapar ett File-objekt
        File directory = new File(directoryPath);

        // Kontrollerar om katalogen finns
        if (directory.exists() && directory.isDirectory()) {
            System.out.println("Katalogen finns.");
        } else {
            System.out.println("Katalogen finns inte.");
        }
    }
}
```
**Exempel på utmatning**:
```
Katalogen finns.
```
Eller
```
Katalogen finns inte.
```

**Använda Tredjepartsbibliotek**:

Även om standardbiblioteket för Java vanligtvis räcker för denna uppgift, erbjuder tredjepartsbibliotek som Apache Commons IO ytterligare filhanteringsverktyg som kan vara användbara i mer komplexa applikationer.

**Apache Commons IO**:

Först lägg till beroendet för Apache Commons IO i ditt projekt. Sedan kan du använda dess funktioner för att kontrollera en katalogs existens.

```java
// Antag att Apache Commons IO har lagts till i projektet

import org.apache.commons.io.FileUtils;

public class DirectoryExistsCommons {
    public static void main(String[] args) {
        // Ange sökvägen till katalogen här
        String directoryPath = "path/to/directory";

        // Använder FileUtils för att kontrollera
        boolean directoryExists = FileUtils.directoryContains(new File(directoryPath), null);

        if (directoryExists) {
            System.out.println("Katalogen finns.");
        } else {
            System.out.println("Katalogen finns inte.");
        }
    }
}
```

**Obs**: `FileUtils.directoryContains` kontrollerar om en katalog innehåller en specifik fil, men genom att skicka `null` som det andra argumentet kan du använda den för att kontrollera katalogens existens. Var försiktig, eftersom detta kanske inte är det mest direktföregående eller avsedda användningssättet för metoden.

**Exempel på utmatning**:
```
Katalogen finns.
```
Eller
```
Katalogen finns inte.
```
