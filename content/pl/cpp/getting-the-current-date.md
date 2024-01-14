---
title:    "C++: Otrzymywanie aktualnej daty"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Dlaczego

Dzisiejsza data jest ważnym elementem w wielu programach. Często musimy uzyskać aktualny dzień, miesiąc lub rok, aby przeprowadzić odpowiednie obliczenia lub wyświetlić informacje dla użytkownika. W tym krótkim artykule dowiesz się, jak w prosty sposób uzyskać aktualną datę w języku C++, aby móc wykorzystać ją w swoich projektach.

## Jak To Zrobić

W języku C++ istnieje wiele sposobów na uzyskanie aktualnej daty. Jednym z najpopularniejszych jest użycie biblioteki <ctime>, która zawiera funkcje umożliwiające operowanie na czasie. Poniżej znajdują się dwa przykłady kodu, które wykorzystują bibliotekę <ctime> do uzyskania aktualnej daty:

```
#include <iostream>
#include <ctime>
using namespace std;

int main() {
    // Przykład 1 - użycie funkcji time()
    time_t now = time(0);
    tm *local_time = localtime(&now);

    cout << "Aktualna data: " << local_time->tm_mday << "-" << local_time->tm_mon + 1 << "-" << local_time->tm_year + 1900;

    // Przykład 2 - użycie funkcji ctime()
    time_t now = time(0);
    char* current_time = ctime(&now);

    cout << "Aktualna data: " << current_time;

    return 0;
}
```

W powyższych przykładach użyto funkcji "time()", która zwraca aktualny czas jako liczbę sekund od 1 stycznia 1970 roku oraz funkcji "localtime()" i "ctime()", które konwertują tę liczbę na czytelną dla człowieka formę daty.

### Wyjście z Przykładu 1:

> Aktualna data: 23-09-2021

### Wyjście z Przykładu 2:

> Aktualna data: Thu Sep 23 14:25:10 2021

Ważne jest również zauważenie, że przy użyciu funkcji "localtime()" musimy odjąć wartość "tm_mon" i "tm_year" o odpowiednie wartości, ponieważ w przypadku tych funkcji miesiące są numerowane od 0, a lata od 1900.

## Głębszy Wgląd

Jeśli chcesz poznać więcej sposobów na uzyskanie aktualnej daty w języku C++ lub dowiedzieć się więcej o funkcjach biblioteki <ctime>, możesz zajrzeć do dokumentacji języka lub poszukać tutoriali online. Warto również pamiętać o tym, że istnieją także dostępne biblioteki zewnętrzne, które ułatwiają operowanie na czasie w języku C++. Dzięki nim możemy np. wygodnie wyświetlać datę w różnych formatach lub wykonywać bardziej skomplikowane operacje na czasie.

## Zobacz Również

- Dokumentacja języka C++: https://en.cppreference.com/w/
- Poradnik na stronie cppdigger.com: https://cppdigger.com/cpp-reference/ctime
- Biblioteka "date" dostępna wraz ze standardem C++20: https://en.cppreference.com/w/cpp/header/date