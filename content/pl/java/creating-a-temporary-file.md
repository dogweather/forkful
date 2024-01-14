---
title:                "Java: Tworzenie tymczasowego pliku"
simple_title:         "Tworzenie tymczasowego pliku"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Tworzenie plików tymczasowych jest często niezbędne w programowaniu w języku Java. Pliki tymczasowe są przeznaczone do przechowywania danych lub informacji, które są potrzebne tylko w określonym momencie i nie będą już używane później. Mogą być również wykorzystywane do testowania aplikacji lub do przechowywania danych, które nie są istotne dla działania programu, ale są potrzebne do jego poprawnego funkcjonowania.

## Jak to zrobić

Aby utworzyć tymczasowy plik w języku Java, można skorzystać z klasy "java.io.File". Najpierw należy utworzyć obiekt tej klasy, określając nazwę pliku, ścieżkę oraz dodatkowe informacje, jeśli są potrzebne. Następnie można wywołać metodę "createNewFile()", która utworzy nowy plik tymczasowy. Przykładowy kod wyglądałby następująco:

```Java
import java.io.File;

public class TemporaryFileExample {

  public static void main(String[] args) {
    // utworzenie obiektu klasy File z nazwą i ścieżką pliku
    File tempFile = new File("temp.txt");
    
    try {
      // wywołanie metody createNewFile(), która utworzy tymczasowy plik
      tempFile.createNewFile();
      System.out.println("Plik tymczasowy został utworzony.");
    } catch (Exception e) {
      System.out.println("Wystąpił błąd: " + e.getMessage());
    }
  }
}
```

Po wykonaniu powyższego kodu w folderze, w którym znajduje się projekt, zostanie utworzony plik o nazwie "temp.txt".

## Głębszy wgląd

Tworzenie plików tymczasowych może być szczególnie przydatne w przypadku przetwarzania dużych ilości danych lub w aplikacjach, które wymagają wielokrotnego dostępu do tych danych. Pliki tymczasowe pozwalają na przechowywanie danych w pamięci masowej, co może znacznie przyspieszyć działanie programu. Ponadto, gdy pliki tymczasowe nie są już potrzebne, mogą zostać łatwo usunięte, aby zwolnić przestrzeń na dysku.

W języku Java istnieje również możliwość utworzenia tymczasowego pliku o określonej zawartości. Aby to zrobić, można skorzystać z klasy "java.nio.file.Files". Przykładowy kod wyglądałby następująco:

```Java
import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

public class TemporaryFileExample {

  public static void main(String[] args) {
    // utworzenie obiektu klasy File z nazwą i ścieżką pliku
    File tempFile = new File("temp.txt");
    
    try {
      // utworzenie ścieżki do pliku
      Path tempFilePath = Paths.get(tempFile.getPath());
      // utworzenie zawartości pliku
      String content = "To jest zawartość tymczasowego pliku.";
      // wywołanie metody write(), która zapisze zawartość do pliku
      Files.write(tempFilePath, content.getBytes());
      System.out.println("Plik tymczasowy został utworzony z określoną zawartością.");
    } catch (Exception e) {
      System.out.println("Wystąpił błąd: " + e.getMessage());
    }
  }
}
```

W powyższym przykładzie, po wykonaniu kodu, plik "temp.txt" zostanie utworzony z zawartością "To jest zawartość tymczasowego pliku.".

## Zobacz również

- Dokumentacja: [https://docs.oracle.com/javase/8/docs/api/java/io/File.html](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- Poradnik: [https://kodejava.org/how-do-i-create-a-temporary-file/](https://kodejava.org