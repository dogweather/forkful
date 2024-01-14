---
title:    "C++: Uzyskiwanie bieżącej daty"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zastanawiałeś się, jak w prosty sposób uzyskać aktualną datę w swoim programie w języku C++? Może to być przydatne, jeśli tworzysz aplikację, która wymaga znalezienia daty lub po prostu chcesz wyświetlić bieżącą datę dla użytkownika. W tym blogu dowiesz się, jak użyć wbudowanej funkcji C++ do pobrania aktualnej daty.

## Jak To Zrobić

Aby uzyskać aktualną datę w języku C++, musisz użyć funkcji `localtime()` z biblioteki `ctime`. Następnie zmiennej typu `time_t` przypiszemy wartość `time(nullptr)`, która zwraca czas w sekundach od 1 stycznia 1970 roku. Następnie, wywołując funkcję `localtime()` z argumentem typu `time_t`, można uzyskać strukturę `tm`, która zawiera informacje o bieżącej dacie i godzinie.

```C++
#include <iostream>
#include <ctime>
using namespace std;

int main() {
    // uzyskanie aktualnego czasu w sekundach
    time_t now = time(nullptr);
    
    // przypisanie czasu do struktury tm
    tm * current = localtime(&now);
    
    // wyświetlenie bieżącej daty
    cout << "Aktualna data: " << current->tm_mday << "." << current->tm_mon+1 << "." << current->tm_year+1900 << endl;
    
    return 0;
}
```

Powinniśmy uzyskać następujący wynik:

```shell
Aktualna data: 5.9.2021
```

## Deep Dive

Funkcja `localtime()` zwraca wskaźnik do struktury `tm`, która zawiera informacje o czasie w następujący sposób:

```C++
struct tm {
    int tm_sec;   // liczba sekund po minucie (od 0 do 61)
    int tm_min;   // liczba minut po godzinie (od 0 do 59)
    int tm_hour;  // liczba godzin po północy (od 0 do 23)
    int tm_mday;  // dzień miesiąca (od 1 do 31)
    int tm_mon;   // liczba miesięcy od stycznia (od 0 do 11)
    int tm_year;  // liczba lat od 1900
    int tm_wday;  // dzień tygodnia (od 0 - niedziela, do 6 - sobota)
    int tm_yday;  // dzień roku (od 0 do 365)
    int tm_isdst; // flaga określająca, czy obowiązuje czas letni
};
```

Możesz również użyć funkcji `mktime()` do konwertowania struktury `tm` na czas lokalny, jeśli na przykład potrzebujesz porównać daty lub wyświetlić je w innym formacie.

## Zobacz też

- [Referencja języka C++: localtime()](https://www.cplusplus.com/reference/ctime/localtime/)
- [Referencja języka C++: mktime()](https://www.cplusplus.com/reference/ctime/mktime/) 
- [Porównaj daty w języku C++](https://www.techiedelight.com/compare-dates-cpp/)