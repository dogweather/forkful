---
title:    "Java: Pisanie pliku tekstowego"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego pisać pliki tekstowe?

Pisanie plików tekstowych jest nieodłączną częścią procesu tworzenia oprogramowania. Jest to nie tylko niezbędne do przechowywania i przesyłania danych, ale także pozwala na czytelne i uporządkowane przechowywanie informacji. Pliki tekstowe są również łatwe do przetwarzania i dostosowywania przez programy komputerowe. W tym artykule dowiesz się, jak napisać plik tekstowy w języku Java.

## Jak to zrobić?

Aby napisać plik tekstowy w języku Java, musisz wykonać kilka prostych kroków:

1. Utwórz obiekt klasy `File` i podaj mu nazwę i ścieżkę, w której ma być utworzony plik tekstowy.
2. Utwórz obiekt klasy `FileWriter` i przekaż do niego obiekt klasy `File` jako parametr konstruktora.
3. Użyj metody `write()` w obiekcie `FileWriter`, aby wpisać tekst do pliku.
4. Pamiętaj o zamykaniu obiektów `FileWriter` i `File` przy użyciu metody `close()`.

Poniższy kod przedstawia przykładową implementację pisanie pliku tekstowego w języku Java:

```Java
import java.io.*;

public class WriteTextFileExample {

    public static void main(String[] args) {
        try {
            File file = new File("C:\\Users\\Użytkownik\\Desktop\\example.txt");
            FileWriter writer = new FileWriter(file); // utworzenie obiektu FileWriter
            writer.write("To jest przykładowy tekst"); // wpisanie tekstu do pliku
            
            writer.close(); // zamknięcie obiektu FileWriter
        } catch (IOException e) {
            System.out.println("Wystąpił błąd: " + e.getMessage());
        }
    }
}
```

Po wykonaniu tego kodu, na pulpicie powinien pojawić się plik o nazwie "example.txt" zawierający wpisany tekst.

## Głębsze zagłębienie

W języku Java istnieje wiele sposobów na pisanie plików tekstowych. Przykład przedstawiony powyżej to tylko jedna z możliwości. Możesz również użyć klasy `PrintWriter` lub `BufferedWriter` do wpisywania tekstu do pliku. Istnieją również metody do przeczytania danych z pliku tekstowego, takie jak `Scanner` lub `BufferedReader`.

Pamiętaj, że przy pisaniu plików tekstowych należy pamiętać o obsłudze wyjątków, aby zapobiec błędom podczas wykonywania operacji na pliku.

## Zobacz także
- [Oficjalna dokumentacja Javy dla klasy File](https://docs.oracle.com/javase/7/docs/api/java/io/File.html)
- [Przykładowe kody dla pisania plików tekstowych w Javie](https://github.com/taufiq0531/Java-WriteTextFile)
- [Podstawy programowania w języku Java](https://pl.wikibooks.org/wiki/Java/Podstawy)