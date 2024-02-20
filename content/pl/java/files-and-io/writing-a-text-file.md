---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:20.611025-07:00
description: "Zapisywanie pliku tekstowego w Javie dotyczy wykorzystywania mo\u017C\
  liwo\u015Bci j\u0119zyka do tworzenia plik\xF3w i zapisywania w nich tre\u015Bci\
  \ na systemie plik\xF3w.\u2026"
lastmod: 2024-02-19 22:04:54.426130
model: gpt-4-0125-preview
summary: "Zapisywanie pliku tekstowego w Javie dotyczy wykorzystywania mo\u017Cliwo\u015B\
  ci j\u0119zyka do tworzenia plik\xF3w i zapisywania w nich tre\u015Bci na systemie\
  \ plik\xF3w.\u2026"
title: Pisanie pliku tekstowego
---

{{< edit_this_page >}}

## Co i dlaczego?

Zapisywanie pliku tekstowego w Javie dotyczy wykorzystywania możliwości języka do tworzenia plików i zapisywania w nich treści na systemie plików. Programiści robią to z różnych powodów, takich jak logowanie, eksportowanie danych lub zapisywanie stanu aplikacji do późniejszego odzyskania.

## Jak to zrobić:

### Używając `java.nio.file` (Biblioteka standardowa)

Pakiet New I/O (NIO) Javy (`java.nio.file`) oferuje bardziej wszechstronne podejście do pracy z plikami. Oto uproszczony sposób na zapis do pliku przy użyciu `Files.write()`:

```java
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;

public class TextFileWriterNIO {
    public static void main(String[] args) {
        List<String> linie = Arrays.asList("Linia 1", "Linia 2", "Linia 3");
        try {
            Files.write(Paths.get("przyklad.txt"), linie);
            System.out.println("Plik zapisany pomyślnie!");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

Wynik:

```
Plik zapisany pomyślnie!
```

### Używając `java.io` (Biblioteka standardowa)

Dla bardziej tradycyjnego podejścia, `java.io.FileWriter` jest dobrym wyborem do prostego zapisywania plików tekstowych:

```java
import java.io.FileWriter;
import java.io.IOException;

public class TextFileWriterIO {
    public static void main(String[] args) {
        try (FileWriter writer = new FileWriter("przyklad.txt")) {
            writer.write("Witaj, Świecie!\n");
            writer.append("To jest kolejna linia.");
            System.out.println("Plik zapisany pomyślnie!");
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

Wynik:

```
Plik zapisany pomyślnie!
```

### Używając Apache Commons IO

Biblioteka Apache Commons IO upraszcza wiele operacji, w tym zapis do pliku. Oto jak zapisać do pliku przy użyciu `FileUtils.writeStringToFile()`:

Najpierw dodaj zależność do swojego projektu. Jeśli używasz Maven, dołącz:

```xml
<dependency>
  <groupId>org.apache.commons</groupId>
  <artifactId>commons-io</artifactId>
  <version>2.11.0</version> <!-- Sprawdź najnowszą wersję -->
</dependency>
```

Następnie użyj następującego kodu, aby zapisać tekst do pliku:

```java
import org.apache.commons.io.FileUtils;
import java.io.File;
import java.io.IOException;

public class TextFileWriterCommonsIO {
    public static void main(String[] args) {
        try {
            FileUtils.writeStringToFile(new File("przyklad.txt"), "To jest tekst zapisany przy użyciu Commons IO.", "UTF-8");
            System.out.println("Plik zapisany pomyślnie!");
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}

```

Wynik:

```
Plik zapisany pomyślnie!
```
