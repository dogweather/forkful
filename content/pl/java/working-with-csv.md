---
title:                "Praca z plikami csv"
html_title:           "Java: Praca z plikami csv"
simple_title:         "Praca z plikami csv"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/working-with-csv.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach, praca z ogromnymi danymi stała się nieodłączną częścią wielu dziedzin, począwszy od biznesu po naukę danych. Jednym z powszechnie stosowanych formatów danych jest plik CSV (ang. Comma-Separated Values), który pozwala na przechowywanie i przetwarzanie danych w postaci tabeli. W tym artykule dowiesz się, dlaczego warto poznać i umiejętnie wykorzystywać CSV w swoich projektach opartych na języku Java.

## Jak to zrobić

Pierwszym krokiem jest zaimportowanie klasy CSVReader z pakietu opencsv do Twojego projektu.

```Java
import com.opencsv.CSVReader;
```

Następnie, musisz utworzyć obiekt CSVReader, który przyjmuje jako parametry plik CSV oraz znak, którym są oddzielone kolumny (zwykle jest to przecinek).

```Java
CSVReader reader = new CSVReader(new FileReader("dane.csv"), ',');
```

Teraz możesz rozpocząć wczytywanie danych z pliku. W tym przykładzie, użyjemy pętli for-each do przeglądania kolejnych wierszy i kolumn w pliku CSV.

```Java
String[] line;

while ((line = reader.readNext()) != null) {
    for (String column : line) {
        System.out.println(column);
    }
}
```

Przykładowy plik CSV "dane.csv" mógłby wyglądać tak:

```
Imię,Nazwisko,Wiek
Jan,Kowalski,35
Anna,Nowak,28
Piotr,Kowalczyk,42
```

W rezultacie, wyświetlona zostałaby kolejno wartości z kolumn "Imię", "Nazwisko" oraz "Wiek" dla każdego wiersza.

```
Jan
Kowalski
35
Anna
Nowak
28
Piotr
Kowalczyk
42
```

## Deep Dive

Powyższe przykłady pokazują najprostsze metody wykorzystania CSVReader. Jednak, istnieje wiele innych funkcji i opcji, które mogą ułatwić pracę z plikami CSV w języku Java.

Na przykład, możesz użyć metody readAll, aby wczytać wszystkie dane z pliku do Listy, dzięki czemu łatwiej będzie Ci operować na całym zestawie danych.

```Java
List<String[]> lines = reader.readAll();
```

Możesz także zastosować różne ustawienia odnośnie separatora kolumn, znaku końca linii czy nawet kodowania znaków.

```Java
CSVReader reader = new CSVReaderBuilder(new FileReader("dane.csv"))
                        .withSeparator(';')
                        .withSkipLines(1)
                        .withEncoding("UTF-8")
                        .build();
```

Jeśli chcesz wprowadzić zmiany w pliku CSV, możesz użyć klasy CSVWriter z tego samego pakietu opencsv.

```Java
CSVWriter writer = new CSVWriter(new FileWriter("nowe_dane.csv"), ',');
```

Następnie, możesz wykorzystać metodę writeNext, aby dodać nowy wiersz do pliku.

```Java
String[] newRow = {"Katarzyna", "Wiśniewska", "24"};
writer.writeNext(newRow);
```

Dodatkowo, opencsv umożliwia także odczyt danych z pliku CSV do obiektów zdefiniowanych przez użytkownika w sposób automatyczny, dzięki wykorzystaniu adnotacji.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o pracy z plikami CSV w języku Java, polecam przeczytać dokumentację pakietu opencsv oraz poniższe artykuły:

- [Jak pracować z plikami CSV w języku Java](https://www.baeldung.com/java-csv)
- [Pliki CSV w języku Java – sposoby odczytu danych](https://www.sam