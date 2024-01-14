---
title:                "C++: Porównywanie dwóch dat"
programming_language: "C++"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Porównywanie dwóch dat jest ważnym aspektem wielu programów i aplikacji. W C++, porównywanie dat może być szczególnie przydatne, na przykład w celu sortowania lub filtrowania danych. W tym blogu wyjaśnimy, jak porównywać daty za pomocą języka C++ i jak to zrobić prawidłowo.

## Jak to zrobić

Porównywanie dat w języku C++ może być wyraźne i proste, jeśli zastosujemy odpowiednie metody. Przedstawimy teraz przykładowy kod i wynik, aby pokazać, jak porównywać daty w C++:

```C++
#include <iostream>
#include <ctime>
 
int main()
{
    // tworzenie dwóch dat
    std::tm d1 = { 0, 0, 0, //sekundy, minuty, godziny
                   1,  // dzień
                   0,  // miesiąc
                   118 }; // rok (od 1900 roku)
    std::tm d2 = { 0, 0, 0, //sekundy, minuty, godziny
                   1,  // dzień
                   0,  // miesiąc
                   120 }; // rok (od 1900 roku)
 
    // konwertowanie dat do typu time_t
    std::time_t t1 = std::mktime(&d1);
    std::time_t t2 = std::mktime(&d2);
 
    // porównywanie dat i wyświetlanie wyniku
    if (t1 < t2)
        std::cout << "Data 1 jest wcześniejsza niż Data 2";
    else if (t1 > t2)
        std::cout << "Data 2 jest wcześniejsza niż Data 1";
    else
        std::cout << "Obie daty są takie same";
 
    return 0;
}
```

```
Output:
Data 1 jest wcześniejsza niż Data 2
```

W powyższym przykładzie używamy funkcji `mktime`, aby skonwertować daty do typu `time_t`, który może być porównywany przy użyciu operatorów logicznych. Należy również pamiętać, że miesiące są indeksowane od 0, więc styczeń ma wartość 0, luty 1 itd.

## Glebokie przedmiot

W języku C++ można również użyć klasy `std::chrono::system_clock` do porównywania dat. Klasa ta zawiera wiele przydatnych funkcji, takich jak `now()`, która zwraca aktualny czas systemowy. Przykładowy kod wykorzystujący tę klasę może wyglądać następująco:

```C++
#include <iostream>
#include <chrono>
 
int main()
{
    // tworzenie dwóch punktów w czasie
    std::chrono::system_clock::time_point tp1 = std::chrono::system_clock::now();
    std::chrono::system_clock::time_point tp2 = tp1 + std::chrono::minutes(5);
 
    // porównywanie czasu
    if (tp1 < tp2)
        std::cout << "Czas 1 jest wcześniejszy niż Czas 2";
    else if (tp1 > tp2)
        std::cout << "Czas 2 jest wcześniejszy niż Czas 1";
    else
        std::cout << "Oba czasy są takie same";
 
    return 0;
}
```

W powyższym przykładzie używamy funkcji `now()` do uzyskania aktualnego czasu systemowego, a następnie używamy operatora `+` do dodania 5 minut do czasu. Należy również pamiętać, że porównywanie punktów w czasie odbywa się na podstawie liczby sekund od początku epoki (1 stycznia 1970 roku).

## Zobacz również

- [Dokumentacja C++ dla funkcji mktime](https://www.cplusplus.com/reference/ctime/mktime/)
- [D