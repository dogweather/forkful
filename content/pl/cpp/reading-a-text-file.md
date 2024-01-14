---
title:                "C++: Odczytywanie pliku tekstowego"
simple_title:         "Odczytywanie pliku tekstowego"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy zdarzyło Ci się kiedyś chcieć przeczytać dane z pliku tekstowego w swoim programie w C++, ale nie wiedziałeś, jak to zrobić? Lub czy potrzebowałeś informacji z pliku tekstowego, ale nie wiesz jak to zaimplementować? W tym artykule dowiesz się, jak w prosty sposób odczytać dane z pliku tekstowego w C++.

## Jak to zrobić

W C++ istnieje kilka sposobów na odczytanie danych z pliku tekstowego. Jedną z najprostszych i najpopularniejszych metod jest użycie obiektu typu `ifstream` z biblioteki `fstream`. Aby tego dokonać, musimy najpierw utworzyć obiekt `ifstream` i otworzyć nasz plik tekstowy przy użyciu metody `open()`. Następnie możemy wykorzystać pętlę `while` do odczytania wszystkich linii z pliku. 
```C++
#include <iostream>
#include <fstream>
using namespace std;

int main() {
    // tworzymy obiekt ifstream
    ifstream plik;
    // otwieramy plik tekstowy
    plik.open("tekst.txt");
    // sprawdzamy, czy plik został otwarty poprawnie
    if(!plik) {
        cerr << "Nie udało się otworzyć pliku!" << endl;
        return 1;
    }
    // zmienna do przechowywania odczytanych linii
    string linia;
    // odczytujemy dane za pomocą pętli while
    while(getline(plik, linia)) { 
        cout << linia << endl;
    }
    // zamykamy plik po zakończeniu odczytywania
    plik.close();
    
    return 0;
}
```
Przykładowy plik tekstowy `tekst.txt`:
```
To jest pierwsza linia tekstu.
To jest druga linia tekstu.
A to jest trzecia i ostatnia linia tekstu.
```
Przykładowy wynik działania programu:
```
To jest pierwsza linia tekstu.
To jest druga linia tekstu.
A to jest trzecia i ostatnia linia tekstu.
```
Żeby jeszcze bardziej ułatwić sobie pracę, możemy wykorzystać także funkcję `get()` do odczytania poszczególnych znaków z pliku. W tym przypadku, musimy wykorzystać pętlę `while` i warunek `eof()` do sprawdzenia, czy przeczytaliśmy już cały plik.
```C++
#include <iostream>
#include <fstream>
using namespace std;

int main() {
    // tworzymy obiekt ifstream
    ifstream plik;
    // otwieramy plik tekstowy
    plik.open("tekst.txt");
    // sprawdzamy, czy plik został otwarty poprawnie
    if(!plik) {
        cerr << "Nie udało się otworzyć pliku!" << endl;
        return 1;
    }
    // zmienna do przechowywania odczytanego znaku
    char znak;
    // odczytujemy znaki za pomocą pętli while
    while(plik.get(znak) && !plik.eof()) { 
        cout << znak;
    }
    // zamykamy plik po zakończeniu odczytywania
    plik.close();
    return 0;
}
```
Przykładowy wynik działania programu:
```
To jest pierwsza linia tekstu.
To jest druga linia tekstu.
A to jest trzecia i ostatnia linia tekstu.
```

## Deep Dive

Oprócz metod `getline()` i `get()`, istnieje jeszcze wiele innych sposobów na odczytanie danych z pliku tekstowego w C++. Możemy także wykorzystać pętle `for`, łączenie znaków do stringa lub funkcję `read()` z biblioteki `istream`. Dodatkowo, istnieją również różne flagi, które można