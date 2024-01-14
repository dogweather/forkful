---
title:    "Java: Tworzenie pliku tymczasowego"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Tworzenie plików tymczasowych jest częstym wyzwaniem w wielu projektach programistycznych. Te pliki są niezbędne do tymczasowego przechowywania danych i zapewnienia wydajności aplikacji podczas wykonywania operacji. W tym wpisie dowiesz się, dlaczego warto tworzyć pliki tymczasowe i jak można to zrobić w języku Java.

## Jak to zrobić

Aby utworzyć plik tymczasowy w języku Java, musimy skorzystać z metody `Files.createTempFile()` z biblioteki `java.nio.file`. W poniższym kodzie możesz zobaczyć przykładową implementację tworzenia pliku tymczasowego oraz jego zawartości.

```Java
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

public class TempFileExample {
    public static void main(String[] args) throws IOException{

        // Tworzenie pliku tymczasowego
        Path tempFile = Files.createTempFile("example", ".txt");

        // Pobranie ścieżki do pliku
        System.out.println("Plik tymczasowy znajduje się w: " + tempFile);

        // Dodanie zawartości do pliku
        String content = "To jest przykładowa zawartość pliku tymczasowego";
        Files.write(tempFile, content.getBytes());

        // Wyświetlenie zawartości pliku
        System.out.println("Zawartość pliku:");
        Files.lines(tempFile).forEach(System.out::println);

        // Usunięcie pliku po zakończeniu pracy
        tempFile.toFile().deleteOnExit();
    }
}
```

Po uruchomieniu powyższego kodu, powinniśmy uzyskać następujący wynik:

```
Plik tymczasowy znajduje się w: /tmp/example2483786501452730714.txt
Zawartość pliku:
To jest przykładowa zawartość pliku tymczasowego
```

Jak widać, plik tymczasowy został utworzony w lokalizacji systemowej określonej przez nasz system operacyjny. W przykładzie wykorzystaliśmy rozszerzenie `.txt`, ale możemy również użyć dowolnego innego rozszerzenia lub nawet nie podawać go w ogóle.

## Głębszy przegląd

W języku Java możemy również dokonać zaawansowanych operacji na plikach tymczasowych, takich jak ustalenie lokalizacji, wymuszanie unikalnej nazwy pliku czy utworzenie własnych plików tymczasowych z użyciem klasy `TempFileHelper`. Więcej informacji na ten temat można znaleźć w [dokumentacji Java](https://docs.oracle.com/javase/8/docs/api/java/io/File.html#createTempFile-java.lang.String-java.lang.String-), gdzie przedstawione są wszystkie dostępne metody i ich parametry.

## Zobacz także

- [Pliki tymczasowe w języku Java - poradnik od Oracle](https://docs.oracle.com/javase/8/docs/api/java/io/File.html#createTempFile-java.lang.String-java.lang.String-)
- [Tworzenie tymczasowych plików w Javie](https://www.baeldung.com/java-temporary-file)
- [Klasy do obsługi plików w języku Java](https://www.geeksforgeeks.org/file-handling-java-using-filewriter-filereader/)