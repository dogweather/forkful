---
title:                "Odczyt pliku tekstowego"
html_title:           "C++: Odczyt pliku tekstowego"
simple_title:         "Odczyt pliku tekstowego"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego
 
Czy kiedykolwiek zastanawiałeś się, jak działa odczytywanie plików tekstowych w języku C++? Pisanie programów, które mogą odczytywać dane z plików tekstowych jest niezbędne w wielu projektach programistycznych. W tym artykule przekażemy Ci kilka prostych wskazówek, jak to zrobić w sposób prosty i skuteczny.
 
## Jak to zrobić
 
Aby odczytać plik tekstowy w języku C++, używamy funkcji `ifstream`, która jest dostępna w bibliotece `fstream`. Najpierw musimy zdefiniować obiekt typu `ifstream` i podać nazwę pliku, który chcemy odczytać. Następnie możemy użyć pętli `while`, aby odczytywać linia po linii, używając funkcji `getline` i wyświetlać je w konsoli. Poniższy przykład pokazuje, jak to zrobić:
 
```C++
#include <iostream>
#include <fstream>
using namespace std;
 
int main() {
    ifstream plik("tekst.txt"); // zdefiniowanie obiektu ifstream z plikiem do odczytu 
    string linia;
    
    while (getline(plik, linia)) { // dopóki są kolejne linie, odczytaj i wyświetl je w konsoli
        cout << linia << endl;
    }
    
    plik.close(); // zamknięcie pliku po zakończeniu pracy z nim
    return 0;
}
```
 
Wyjście dla pliku `tekst.txt`:
```
To jest przykładowa linia tekstu.
To jest kolejna linia tekstu.
A to jest jeszcze jedna linia tekstu.
```
 
Możemy również użyć funkcji `get` i `put` do odczytywania i zapisywania pojedynczych znaków w pliku. Poniższy przykład pokazuje to na prostym słowniku:
 
```C++
#include <iostream>
#include <fstream>
using namespace std;
 
int main() {
    ifstream plik("slownik.txt"); // zdefiniowanie obiektu ifstream z plikiem do odczytu
    string slowo;
 
    cout << "Podaj słowo, aby znaleźć jego definicję: ";
    cin >> slowo;
    
    char definicja[50];
    int i = 0;
    
    while (plik >> definicja[i]) { // dopóki są kolejne znaki, zapisz je w tablicy
        i++;
    }
    
    cout << slowo << ": " << definicja << endl; // wyświetlenie słowa i jego definicji
    
    plik.close(); // zamknięcie pliku po zakończeniu pracy z nim
    return 0;
}
```
 
Wyjście dla pliku `slownik.txt`:
```
programowanie
|s|ł|i|c|a| op|ro|g|ra|mo|wa|ni|e
```
 
## Deep Dive
 
Funkcja `ifstream` jest jedną z wielu dostępnych w bibliotece `fstream`. Pozwala nam na odczytywanie danych z plików w prosty sposób, jednak istnieją również inne funkcje, takie jak `ofstream` do zapisywania danych do pliku lub `fstream` do jednoczesnego odczytu i zapisu. 
 
Ważne jest również, aby pamiętać o otwieraniu i zamykaniu pliku, aby uniknąć niepotrzebnych problemów w przetwarzaniu danych. Istnieje również wiele innych metod odczytywania plików tekstowych, takich jak używanie strumieni lub biblioteki `<sstream>`, ale wykorzystanie funkcji `ifstream` jest najprostszym sposobem dla początkujących programistów.
 
## Zobacz również
 
- [Dokumentacja funkcji ifstream w języku C++](https://en.cppreference.com/w/cpp/io/basic_if