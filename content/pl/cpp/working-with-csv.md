---
title:                "Praca z plikami CSV"
html_title:           "C++: Praca z plikami CSV"
simple_title:         "Praca z plikami CSV"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/working-with-csv.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli pracujesz z dużymi zbiorami danych, prawdopodobnie będziesz często spotykać się z plikami CSV (ang. Comma-Separated Values - Wartości oddzielone przecinkami). Może to być dzieje się to w analizie danych, w programowaniu aplikacji lub w tworzeniu raportów. Dlatego ważne jest, aby poznać podstawy obsługi plików CSV w języku C++, aby uprościć swoją pracę z tym formatem.

## Jak to zrobić

Pierwszym krokiem jest załączenie biblioteki `<fstream>` w pliku nagłówkowym i utworzenie strumienia danych do pliku CSV. Następnie, za pomocą pętli while, można odczytywać plik linia po linii i wpisać go do wektora używając funkcji `getline()`. Przykładowy kod wyglądałby następująco:

```C++
#include <iostream>
#include <vector>
#include <fstream>

int main() {
    std::ifstream file("dane.csv"); //otwiera strumień do pliku CSV
    std::string line; //zmienna do przechowywania aktualnie odczytywanej linii
    std::vector<std::string> data; //wektor do przechowywania danych
    while(getline(file, line)) { //odczytaj plik linia po linii i wpisz do zmiennej "line"
        data.push_back(line); //dodaj odczytaną linię do wektora "data"
    }
    for(std::string d : data) { //przejdź przez wszystkie elementy wektora "data"
        std::cout << d << std::endl; //wyświetl dane na ekranie
    }
    return 0;
}
```
Przykładowy plik CSV "dane.csv" mógłby wyglądać tak:

```text
Imię;Nazwisko;Wiek;Email
Anna;Kowalska;30;anna.kowalska@example.com
Adam;Nowak;25;adam.nowak@example.com
Maria;Wiśniewska;35;maria.wisniewska@example.com
```

Po uruchomieniu programu otrzymalibyśmy następujące wyjście:

```text
Imię;Nazwisko;Wiek;Email
Anna;Kowalska;30;anna.kowalska@example.com
Adam;Nowak;25;adam.nowak@example.com
Maria;Wiśniewska;35;maria.wisniewska@example.com
```

## Deep Dive

Pliki CSV są zwykle prostsze w obsłudze niż inne formaty danych, ponieważ można je czytać w prosty sposób linia po linii. Jednak należy uważać na specjalne znaki, takie jak przecinek czy podwójny cudzysłów, które może występować w wartościach. Innym ważnym aspektem jest poprawne odczytywanie typów danych, takich jak liczby czy daty. W takiej sytuacji można skorzystać z gotowych bibliotek, takich jak "csv-parser".

## Zobacz także

- [Biblioteka csv-parser](https://github.com/ben-strasser/fast-cpp-csv-parser)
- [Praca z plikami CSV w języku C++](https://www.tutorialspoint.com/cplusplus/cpp_files_streams.htm)
- [Dokumentacja biblioteki fstream (pl)](https://pl.cppreference.com/w/cpp/io/basic_fstream)