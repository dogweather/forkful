---
title:                "C++: Konwersja daty na ciąg znaków"
programming_language: "C++"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Często w programowaniu musimy konwertować różne typy danych, takie jak liczby, na tekstowe znaki. W tym przypadku, konwersja daty na ciąg znaków może być przydatna, gdy chcemy wyświetlić datę w czytelnej formie dla użytkownika lub zapisać ją w pliku tekstowym. W tym artykule dowiesz się, jak wykonać tę konwersję w języku C++.

## Jak to zrobić

Aby przekonwertować datę na ciąg znaków w C++, użyjemy funkcji `strftime()` z biblioteki `ctime`. Oto przykładowy kod:

```
#include <iostream>
#include <ctime>

int main()
{
    // Ustawienie aktualnej daty i czasu jako początkowej wartości
    time_t now = time(0);
    
    // Utworzenie bufora do przechowywania wyniku konwersji
    char buffer[80];
    
    // Przekonwertowanie daty do ciągu znaków i zapisanie jej w buforze
    strftime(buffer, sizeof(buffer), "%d.%m.%Y", localtime(&now));
    
    // Wyświetlenie skonwertowanej daty
    std::cout << "Dzisiejsza data to: " << buffer << std::endl;
    
    return 0;
}
```
**Wyjście:**
```
Dzisiejsza data to: 22.05.2021
```

W powyższym przykładzie ustawiliśmy aktualną datę i czas jako początkową wartość, a następnie użyliśmy funkcji `strftime()` do skonwertowania daty do ciągu znaków w formacie "%d.%m.%Y", co oznacza dzień.miesiąc.rok. Możemy zmienić ten format według własnych preferencji, korzystając z różnych specyfikatorów dostępnych w tej funkcji.

## Deep Dive

Funkcja `strftime()` przyjmuje trzy argumenty: bufor, maksymalny rozmiar bufora i formatowanie. Pierwszy argument to wskaźnik na bufor, do którego będzie zapisywana skonwertowana data. Drugi argument to maksymalny rozmiar bufora, który zapobiegnie przepełnieniu pamięci w przypadku dłuższego formatowania. Trzeci argument, czyli formatowanie, jest ciągiem znaków, które określają, w jakim formacie chcemy skonwertować datę. W tym formacie możemy używać różnych specyfikatorów, takich jak %d (dzień), %m (miesiąc) czy %Y (rok), aby dostosować wyjście do naszych potrzeb. Dokładny opis dostępnych specyfikatorów można znaleźć w dokumentacji języka C++.

Ważnym aspektem, o którym należy pamiętać przy konwersji daty do ciągu znaków, jest użycie funkcji `localtime()`, która konwertuje czas zapisany w formacie `time_t` na strukturę `tm`, zawierającą informacje o dacie i godzinie. Funkcja `localtime()` przyjmuje jako argument wskaźnik na czas i zwraca wskaźnik na strukturę `tm`, którą następnie możemy wykorzystać w konwersji do ciągu znaków.

## Zobacz również

- [Dokumentacja języka C++ o funkcji `strftime()`](https://en.cppreference.com/w/cpp/chrono/c/strftime)
- [Tutorial na temat manipulowania datami w C++](https://www.techiedelight.com/manipulating-dates-times-cpp/)
- [Przykłady konwersji daty do ciągu znaków w C++](https://www.geeksforgeeks.org/cpp-program-convert-date-string/)