---
title:                "C++: Obliczanie daty w przyszłości lub przeszłości"
programming_language: "C++"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego

C++ jest jednym z najpopularniejszych języków programowania używanych przez programistów na całym świecie. W dzisiejszych czasach, obecność komputera i technologii stała się nieodłączną częścią naszego codziennego życia. Wielu programistów ma potrzebę tworzenia aplikacji, które odzwierciedlają te potrzeby i ułatwiają nasze codzienne zadania. Liczenie daty w przyszłości lub w przeszłości jest jedną z wielu operacji, które programiści mogą napisać w C++.

## Jak to zrobić

Poniżej przedstawione są przykładowe kody C++ dla liczenia daty w przyszłości lub w przeszłości. Pamiętaj, że kod może różnić się w zależności od środowiska programistycznego lub wersji języka. Przed uruchomieniem kodu upewnij się, że masz odpowiednie narzędzia i dodatki.

### Liczenie daty w przyszłości
```C++
// Importowanie bibliotek
#include <iostream>
#include <chrono>
#include <ctime>
using namespace std;

int main() {
    // Ustalenie daty bieżącej
    auto aktualnaData = chrono::system_clock::now();
    
    // Liczenie daty w przyszłości
    auto dataWPrzyszlosci = aktualnaData + chrono::minutes(30);
    
    // Konwersja na format czytelny dla użytkownika
    time_t dataWPrzyszlosci_t = chrono::system_clock::to_time_t(dataWPrzyszlosci);
    cout << "Data za 30 minut to: " << ctime(&dataWPrzyszlosci_t);
    
    return 0;
}
```

#### Przykładowy wynik:
```
Data za 30 minut to: Thu Dec 17 13:37:05 2020
```

### Liczenie daty w przeszłości
```C++
// Importowanie bibliotek
#include <iostream>
#include <chrono>
#include <ctime>
using namespace std;

int main() {
    // Ustalanie daty bieżącej
    auto aktualnaData = chrono::system_clock::now();
    
    // Liczenie daty w przeszłości
    auto dataWPrzeszlosci = aktualnaData - chrono::hours(1);
    
    // Konwersja na format czytelny dla użytkownika
    time_t dataWPrzeszlosci_t = chrono::system_clock::to_time_t(dataWPrzeszlosci);
    cout << "Data sprzed godziny to: " << ctime(&dataWPrzeszlosci_t);
    
    return 0;
}
```

#### Przykładowy wynik:
```
Data sprzed godziny to: Thu Dec 17 11:36:05 2020
```

## Deep Dive

Przyliczanie daty w przyszłości lub w przeszłości może być wykorzystane w wielu różnych scenariuszach. Może to być użyteczne w tworzeniu harmonogramów, ustawianiu przypomnień czy wyświetlaniu informacji o terminie ważności. W C++, można skorzystać z wielu różnych funkcji i struktur, takich jak `time_t` czy `chrono`, aby dokonać obliczeń związanych z datami.

Jedną z możliwości jest użycie operatora `+` lub `-`, aby dodać lub odjąć odpowiednią liczbę jednostek czasu, takich jak minuty, godziny czy dni. Następnie, wynik jest konwertowany na czytelny dla użytkownika format przy użyciu funkcji `ctime`.

Warto również zwrócić uwagę na fakt, że operacje na danych są wykonywane w czasie bieżącym, co może mieć znaczenie, jeśli wykonujemy obliczenia w określonej strefie czasowej.

## Zobacz też

- [Wykorzystanie biblioteki `chrono`](https://en.cppreference.com/w/cpp/header/chrono)
- [Przykład wykorzystania iume