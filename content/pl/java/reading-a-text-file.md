---
title:                "Java: Odczytywanie pliku tekstowego"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Odczytywanie plików tekstowych jest niezbędnym elementem wielu zadań programistycznych. Sam tekstowy format jest bardzo popularny, ponieważ jest zrozumiały dla ludzi i łatwy do edytowania. W wielu przypadkach, tworząc aplikację, musimy odczytać dane z pliku, aby móc je wyświetlić lub przetworzyć. W tym blogu dowiesz się jak odczytywać pliki tekstowe w języku Java i jak możesz to wykorzystać w swoich projektach.

## Jak To Zrobić

Aby odczytać plik tekstowy w języku Java, musimy użyć klasy FileReader oraz BufferedReader. Najpierw musimy utworzyć obiekt FileReader, który będzie reprezentował nasz plik tekstowy. Następnie, przy użyciu BufferedReader, możemy odczytać zawartość pliku linia po linii. Poniżej znajduje się przykładowy kod, który pokazuje ten proces:

```Java
import java.io.*;

public class ReadFile {
    
    public static void main(String[] args) {
        try {
            // utworzenie obiektu FileReader z plikiem "plik.txt"
            FileReader fileReader = new FileReader("plik.txt");
            
            // utworzenie obiektu BufferedReader z obiektem FileReader
            BufferedReader bufferedReader = new BufferedReader(fileReader);
            
            String line;
            
            // odczytanie pierwszej linii pliku
            while ((line = bufferedReader.readLine()) != null) {
                System.out.println(line);
            }
            
            // zamknięcie BufferedReader
            bufferedReader.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
    
}
```

Powyższy kod wyświetli zawartość pliku "plik.txt" w konsoli.

## Deep Dive

Klasa BufferedReader jest często wykorzystywana przy odczytywaniu plików tekstowych, ponieważ zapewnia wygodny sposób odczytu danych linia po linii. Odpowiednio zamykając ten obiekt za pomocą metody `close()`, możemy również uniknąć wycieku pamięci. Klasy FileReader i BufferedReader współpracują ze sobą, co umożliwia lepszą wydajność oraz możliwość odczytywania większych plików.

Teraz, gdy już wiesz jak odczytywać pliki tekstowe w języku Java, możesz wykorzystać tę wiedzę w swoich projektach, np. przy tworzeniu aplikacji do analizy logów lub eksportowania danych do pliku tekstowego.

## Zobacz Również

- [Dokumentacja klasy BufferedReader w języku Java](https://docs.oracle.com/javase/8/docs/api/java/io/BufferedReader.html)
- [Poradnik: Jak pisać pliki tekstowe w języku Java](https://www.baeldung.com/java-write-to-file)
- [Tutorial: Praca z plikami w języku Java](https://www.tutorialspoint.com/java/io/java_io_file.htm)