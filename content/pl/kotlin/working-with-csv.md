---
title:                "Praca z plikami csv"
html_title:           "Kotlin: Praca z plikami csv"
simple_title:         "Praca z plikami csv"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/working-with-csv.md"
---

{{< edit_this_page >}}

## Czemu CSV?

CSV (Comma-Separated Values) jest popularnym formatem plików używanym do przechowywania danych tabelarycznych. Dzięki prostocie i wszechstronności, jest szeroko stosowany w różnych dziedzinach, takich jak biznes, nauka czy programowanie. Praca z plikami CSV może być niezbędna w wielu projektach, dlatego warto poznać podstawy tego formatu.

## Jak to zrobić?

Zanim przejdziemy do kodowania, musimy zaimportować potrzebne pakiety:

```Kotlin
import java.io.File
import com.github.doyaaaaaken.kotlincsv.dsl.csvReader
```

Teraz możemy zacząć pracę z plikiem CSV. Przykładowy plik "dane.csv" wygląda następująco:

```
imie,nazwisko,wiek
Anna,Kowalska,29
Paweł,Nowak,36
Marta,Nowicka,42
```

Następnie otwieramy plik i wczytujemy jego zawartość używając pakietu csvReader:

```Kotlin
val plik = File("dane.csv")
val zawartosc = csvReader().readAllWithHeader(plik)
```

Teraz mamy dostęp do danych z pliku, możemy je wykorzystać w naszym programie. Przykładowo, aby wyświetlić imiona z naszej listy, możemy użyć pętli for:

```Kotlin
for(item in zawartosc) {
    println(item["imie"])
}
```

Powyższy kod wyświetli:

```
Anna
Paweł
Marta
```

Możemy również zapisywać zmiany w pliku CSV, używając metody "writeAll":

```Kotlin
csvWriter().open("nowy_plik.csv") {
    writeAll(zawartosc)
}
```

## Bardziej zaawansowane informacje

Kotlin oferuje również inne sposoby na pracę z plikami CSV, takie jak wykorzystanie biblioteki "kotlin-csv" czy bezpośrednie wykorzystanie klas z pakietu java.io. Możliwości jest wiele, więc warto poszukać rozwiązań, które najlepiej pasują do naszego projektu.

## Zobacz również

- Dokumentacja pakietu java.io: https://developer.android.com/reference/java/io/package-summary.html
- Biblioteka "kotlin-csv": https://github.com/doyaaaaaken/kotlin-csv
- Szybki przewodnik po formatowaniu CSV: https://www.codecademy.com/articles/what-is-csv