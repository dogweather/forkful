---
title:                "Java: Praca z plikami csv"
simple_title:         "Praca z plikami csv"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/working-with-csv.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach, dane są powszechnie wykorzystywane w różnych dziedzinach, takich jak biznes, nauka i edukacja. Jednym z popularnych formatów przechowywania danych są pliki CSV (ang. comma-separated values), które są wykorzystywane do przechowywania dużych zestawów danych w postaci tabeli. W tym artykule dowiedziesz się, dlaczego warto nauczyć się pracy z CSV w języku programowania Java.

## Jak to zrobić?

Aby pracować z plikami CSV w języku Java, musimy najpierw importować odpowiednie biblioteki. Poniżej przedstawiamy przykładowy kod, który pozwoli nam odczytać plik CSV i wyświetlić jego zawartość w konsoli:

```java
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

public class CSVReader {
    public static void main(String[] args) {
        // ścieżka do pliku CSV
        String filePath = "nazwa_pliku.csv";
        try {
            // wczytanie całego pliku do tablicy
            String[] lines = Files.readAllLines(Paths.get(filePath)).toArray(String[]::new);
            for (String line : lines) {
                // podzielenie linii na poszczególne wartości, korzystając z przecinka jako separatora
                String[] values = line.split(",");
                // wyświetlenie danych w konsoli
                for (String value : values) {
                    System.out.print(value + " ");
                }
                System.out.println();
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

Powyższy kod pozwoli nam odczytać plik CSV i wypisać jego zawartość w konsoli. Należy jednak pamiętać, że w zależności od struktury pliku, konieczne może być dostosowanie kodu oraz wykorzystanie innych narzędzi do pracy z CSV w Javie.

## Głębszy zanurzenie

Istnieje wiele zaawansowanych technik pracy z plikami CSV w języku Java, takich jak wykorzystanie bibliotek Apache Commons CSV czy OpenCSV. Pozwalają one na bardziej precyzyjne odczytywanie i zapisywanie danych, a także na obsługę plików z nagłówkami czy złożonymi strukturami. Warto więc poszerzyć swoją wiedzę na temat pracy z CSV w Javie, aby jeszcze lepiej wykorzystywać ten format w swoich projektach.

## Zobacz również

Ciekawe artykuły i materiały dotyczące pracy z CSV w języku Java:

- [Jak pracować z plikami CSV w Java](https://www.baeldung.com/java-csv)
- [Apache Commons CSV - dokumentacja](https://commons.apache.org/proper/commons-csv/)
- [OpenCSV - dokumentacja](http://opencsv.sourceforge.net/)