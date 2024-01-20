---
title:                "Tworzenie pliku tekstowego"
html_title:           "C++: Tworzenie pliku tekstowego"
simple_title:         "Tworzenie pliku tekstowego"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Pisanie plików tekstowych jest procesem tworzenia plików z tekstem (zwykle z rozszerzeniem .txt), który może być przeczytany i zinterpretowany przez komputer. Programiści korzystają z tego narzędzia, aby przechowywać dane w postaci tekstowej, która jest łatwa do odczytania przez ludzi i komputery.

## Jak to zrobić:
```
// Przykład 1: Tworzenie i zapisanie pliku tekstowego
#include <iostream>
#include <fstream>

using namespace std;

int main() {
    ofstream plik("moj_plik.txt"); // Tworzy nowy plik o nazwie "moj_plik.txt"
    if(!plik) {
        cout << "Nie mozna utworzyc pliku.";
        return 0;
    }
    plik << "To jest zawartosc pliku tekstowego."; // Zapisuje tekst do pliku
    plik.close(); // Zamyka plik
    return 0;
}

// Przykład 2: Dopisywanie tekstu do istniejącego pliku tekstowego
#include <iostream>
#include <fstream>

using namespace std;

int main() {
    ofstream plik("moj_plik.txt", ios::app); // Otwiera plik w trybie dopisywania
    if(!plik) {
        cout << "Nie mozna otworzyc pliku.";
        return 0;
    }
    plik << "Kolejna linia tekstu."; // Dopisuje tekst do istniejącego pliku
    plik.close(); // Zamyka plik
    return 0;
}
```
Przykłady wykorzystują bibliotekę ```<fstream>``` do manipulacji plikami.

## Wnikliwiej:
Początki pisania plików tekstowych sięgają początków programowania komputerów. Obecnie istnieją również inne formaty zapisu danych, takie jak bazy danych czy pliki binarne, jednak pliki tekstowe są wciąż powszechnie wykorzystywane przez programistów ze względu na swoją prostotę i czytelność. W celu zapisania tekstu w pliku, programista może użyć różnych funkcji dostępnych w bibliotece ```<fstream>```.

## Zobacz także:
- [Dokumentacja biblioteki <fstream>](https://en.cppreference.com/w/cpp/header/fstream)
- [Podstawy tworzenia plików w C++](https://www.geeksforgeeks.org/file-handling-c-classes/)