---
title:    "Java: Odczytywanie pliku tekstowego"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Dlaczego

Czy zastanawiałeś się kiedyś, jak w prosty i skuteczny sposób odczytać plik tekstowy w języku Java? W tym artykule dowiesz się, dlaczego jest to ważna umiejętność i jak możesz ją wykorzystać w swoich programach.

## Jak odczytać plik tekstowy w języku Java

Aby odczytać plik tekstowy w języku Java, musimy wykonać kilka prostych kroków:

1. Utworzyć obiekt typu `File` i przekazać do niego ścieżkę do pliku, który chcemy odczytać.
2. Stworzyć obiekt typu `Scanner` i przekazać do niego stworzony wcześniej obiekt `File`.
3. Wykorzystać metodę `nextLine()` obiektu `Scanner`, aby odczytać kolejne linie tekstu z pliku.

Poniżej znajduje się przykładowy kod w języku Java:

```java
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class Main {
  public static void main(String[] args) {
    try {
      File file = new File("tekst.txt");
      Scanner scanner = new Scanner(file);

      // odczytanie i wyświetlenie kolejnych linii z pliku
      while (scanner.hasNextLine()) {
        String line = scanner.nextLine();
        System.out.println(line);
      }

      scanner.close();
    } catch (FileNotFoundException e) {
      System.out.println("Nie znaleziono pliku.");
    }
  }
}
```

Powyższy kod odczytuje i wyświetla zawartość pliku tekstowego "tekst.txt". Możemy również przetwarzać odczytane linie tekstu w inny sposób, na przykład dodając je do listy lub wykorzystując do operacji matematycznych.

## Głębszy wgląd

Podczas odczytywania plików tekstowych w języku Java istnieje kilka ważnych kwestii, które warto znać:

- Możemy wykorzystać różne znaki jako separator przy odczytywaniu danych z pliku tekstowego. Domyślnym separatorem jest znak nowej linii, ale możemy również użyć np. przecinka lub tabulatora.
- Jeśli chcemy odczytywać dane z pliku w formacie innym niż tekst, np. dane liczbowe, musimy odpowiednio przetwarzać odczytane linie za pomocą metod takich jak `parseInt()` lub `parseDouble()`.
- Pamiętajmy, aby zamknąć obiekt typu `Scanner` po zakończeniu jego wykorzystania, aby uniknąć wycieku pamięci.

## Zobacz też

- Dokumentacja języka Java: https://docs.oracle.com/javase/7/docs/api/java/util/Scanner.html
- Przykładowe pliki tekstowe do odczytania: https://github.com/aslamanver/Java-File-Reader/tree/master/example_files