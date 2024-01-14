---
title:                "Java: Tworzenie pliku tekstowego"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

Pisanie tekstu w języku Java: Dlaczego i jak

## Dlaczego

Pisanie tekstu w języku Java jest niezbędnym umiejętnością dla każdego programisty. To podstawowa umiejętność potrzebna do tworzenia aplikacji, gier, a nawet stron internetowych. Bez niej niemożliwe byłoby wyświetlanie informacji użytkownikom lub zapisywanie danych. Pisanie tekstu jest jednym z fundamentalnych kroków do nauki programowania w języku Java. Jest nie tylko przydatne, ale i bardzo ważne w codziennej pracy.

## Jak to zrobić

Aby napisać tekst w języku Java, potrzebujemy tylko kilku wierszy kodu. Załóżmy, że chcemy utworzyć nowy plik tekstowy o nazwie "mojTekst.txt" i zapisać w nim pewien tekst. Oto jak możemy to zrobić:

```
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

public class PisanieTekstu {

  public static void main(String[] args) throws IOException {

    // utworzenie nowego pliku w bieżącym katalogu
    File plik = new File("mojTekst.txt");

    // utworzenie obiektu FileWriter
    FileWriter pisarz = new FileWriter(plik);

    // zapisanie tekstu w pliku
    pisarz.write("To jest tekst zapisany w pliku.");

    // zamknięcie pisarza
    pisarz.close();

    // wyświetlenie komunikatu o powodzeniu
    System.out.println("Tekst został zapisany w pliku.");
  }
}
```

Po uruchomieniu tego kodu, w bieżącym katalogu powinien pojawić się nowy plik o nazwie "mojTekst.txt" z zapisanym w nim tekstem. W ten prosty sposób możemy pisać tekst w języku Java.

## Deep Dive

Przyjrzyjmy się teraz nieco dokładniej poszczególnym krokom używanym w przykładzie powyżej. Na początek, importujemy klasę "File", która pozwala nam tworzyć i manipulować plikami. Następnie, mamy obiekt "FileWriter" - jest to klasa, która pozwala nam zapisywać dane do pliku. Wiesz już, że za pomocą metody "write" możemy zapisać tekst w pliku. Na koniec, musimy zamknąć pisarza, aby zapisać zmiany i uniknąć błędów. I to wszystko!

## Zobacz również

- [Java - Dokumentacja o klasie File](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [Java - Dokumentacja o klasie FileWriter](https://docs.oracle.com/javase/8/docs/api/java/io/FileWriter.html)
- [Wprowadzenie do pisania tekstu w języku Java](https://www.w3schools.com/java/java_files_create.asp)