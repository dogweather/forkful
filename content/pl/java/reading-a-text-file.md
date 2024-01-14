---
title:    "Java: Odczytywanie pliku tekstowego"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zastanawiałeś się, jak programy czytają pliki tekstowe? Niezależnie od tego, czy jesteś początkującym programistą, czy doświadczonym weteranem, umiejętność czytania plików tekstowych jest niezbędna w wielu projektach programistycznych. W tym artykule dowiesz się, dlaczego warto poznać tę umiejętność.

## Jak to zrobić

Jeśli chcesz nauczyć się czytać pliki tekstowe w języku Java, nie martw się! Jest to proste i szybkie, a tu pokażę Ci przykłady kodów, które wykonują te działania. Spróbuj uruchomić je w swoim własnym środowisku programistycznym i zaobserwuj wyniki.

```Java
import java.io.*;

public class ReadFile {
    public static void main(String[] args) {
        // Utwórz obiekt File i podaj ścieżkę do pliku tekstowego
        File file = new File("moj_plik.txt");

        try {
            // Utwórz obiekt FileReader do czytania pliku
            FileReader reader = new FileReader(file);

            // Utwórz obiekt BufferedReader do optymalizacji czytania pliku
            BufferedReader breader = new BufferedReader(reader);

            // Użyj pętli do zapisania każdej linii tekstu jako stringa i wyświetlenia go na konsoli
            String line = breader.readLine();
            while (line != null) {
                System.out.println(line);
                line = breader.readLine();
            }

            // Zamknij obiekty reader i breader
            breader.close();
            reader.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

Przykładowy plik tekstowy `moj_plik.txt` zawiera:

```
Witaj,

Nie ma co czytać słów,
lepiej przeczytaj kod!

Pozdrowienia,
Programista
```

Wyjście na konsoli po uruchomieniu przykładowego kodu będzie wyglądać następująco:

```
Witaj,
Nie ma co czytać słów,
lepiej przeczytaj kod!
Pozdrowienia,
Programista
```

## Głębszy zanurzenie

Teraz, gdy wiesz, jak czytać pliki tekstowe, zastanówmy się na chwilę, jak dokładnie działa ten kod. W naszym przykładzie użyliśmy klasy `FileReader` do czytania pliku i klasy `BufferedReader` do optymalizacji tego procesu. Klasa `BufferedReader` czyta dane w paczkach, co jest szybsze niż czytanie znaku po znaku przez klasę `FileReader`.

W pętli wykorzystujemy metodę `readLine()`, która czyta kolejne linie pliku i zapisuje je jako stringi. Każda linia jest wyświetlana na konsoli, a następnie pętla kontynuuje działanie, dopóki nie dojdzie do końca pliku.

Pamiętaj również o zamknięciu obiektów `reader` i `breader`, aby zwolnić zasoby systemowe.

## Zobacz także

Jeśli chcesz pogłębić swoją wiedzę na temat czytania plików tekstowych w języku Java, polecam przejrzeć poniższe linki:

- [Dokumentacja Java - klasa FileReader](https://docs.oracle.com/javase/7/docs/api/java/io/FileReader.html)
- [Dokumentacja Java - klasa BufferedReader](https://docs.oracle.com/javase/7/docs/api/java/io/BufferedReader.html)
- [Tutorial: czytanie pliku tekstowego w Java](https://www.baeldung.com/java-file-reader)
- [Czytanie i zapisywanie plików w Java](https://www.tutorialspoint.com/java/io/java_io_file.htm)