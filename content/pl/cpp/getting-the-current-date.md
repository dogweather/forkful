---
title:                "C++: Uzyskiwanie bieżącej daty"
programming_language: "C++"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego

Programowanie jest szerokim i wszechstronnym obszarem, który pozwala nam na tworzenie niesamowitych rzeczy. Jedną z ważnych umiejętności potrzebnych do pisania skutecznego i efektywnego kodu jest operowanie na datach. Dlatego, w tym artykule, omówię jak w prosty sposób uzyskać aktualną datę w języku C++.

## Jak to zrobić?

Aby uzyskać bieżącą datę w języku C++, musimy użyć biblioteki stdlib.h i jej funkcji `time()`. Poniżej znajduje się kod demonstrujący jak uzyskać aktualną datę i wyświetlić ją w konsoli.

```C++
#include <iostream>
#include <ctime>

using namespace std;

int main(){

    // Deklarujemy i inicjalizujemy zmienną time_t
    time_t t = time(NULL);
    
    // Używamy funkcji ctime() do zmiany czasu na format czytelny dla człowieka
    cout << "Aktualna data: " << ctime(&t) << endl;

    return 0;
}
```

Wynik powyższego kodu może wyglądać następująco:

```
Aktualna data: Sun Oct 24 00:21:16 2021
```

## Pogłębione spojrzenie

Możemy wykorzystać różne funkcje z biblioteki `ctime` do ulepszenia naszego kodu. Na przykład, można użyć funkcji `localtime()` do uzyskania daty w postaci struktury `tm` zawierającej wszystkie informacje o dacie, takie jak dzień, miesiąc, rok, godzina, minuta, sekunda itp. Następnie możemy wykorzystać te informacje do opracowania własnego formatu daty w zależności od naszych potrzeb.

```C++
#include <iostream>
#include <ctime>

using namespace std;

int main(){

    // Deklarujemy i inicjalizujemy zmienną time_t
    time_t t = time(NULL);

    // Używamy funkcji localtime() do uzyskania daty w postaci struktury tm
    struct tm* now = localtime(&t);

    // Wyświetlamy informacje o dacie w wybranym przez nas formacie
    cout << "Aktualna data: " << now->tm_mday << "/" << now->tm_mon+1 << "/" << 
        1900+now->tm_year << " " << now->tm_hour << ":" << now->tm_min << ":" << 
        now->tm_sec << endl;

    return 0;
}
```

Wynik powyższego kodu dla dzisiejszej daty 24.10.2021 może wyglądać następująco:

```
Aktualna data: 24/10/2021 0:24:16
```

## Zobacz także

- [Dokumentacja standardowej biblioteki języka C++](https://en.cppreference.com/w/cpp/chrono/c)
- [Struktura tm w języku C++](https://www.tutorialspoint.com/c_standard_library/c_function_localtime.htm)
- [Inne funkcje z biblioteki `ctime`](https://www.geeksforgeeks.org/c-tutorial-ctime-cc-and-time-h/)