---
title:    "C: Zdobycie aktualnej daty"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy zastanawiałeś się kiedykolwiek, jak programy na Twoim komputerze potrafią wyświetlić aktualną datę? Programowanie jest sztuką tworzenia kodu, który wykonuje różne zadania - a jednym z takich zadań może być pobieranie aktualnej daty. W tym artykule dowiesz się, jak można łatwo uzyskać bieżącą datę w języku C.

## Jak to zrobić

Aby uzyskać aktualną datę w języku C, możemy skorzystać z biblioteki "time.h". Ta biblioteka zawiera funkcję "time()", która zwraca bieżący czas w sekundach od 1 stycznia 1970 roku. Następnie możemy przetworzyć ten czas i przedstawić go w bardziej przyjaznym dla użytkownika formacie, za pomocą funkcji "localtime()".

Oto przykładowy kod, który pokazuje, jak uzyskać bieżącą datę w formacie dzień-miesiąc-rok:

```C
#include <stdio.h> 
#include <time.h> 

int main() 
{ 
    // pobranie bieżącego czasu
    time_t now = time(NULL); 

    // przetworzenie czasu do lokalnej strefy czasowej
    struct tm *local = localtime(&now); 

    // wyświetlenie bieżącej daty
    printf("Bieżąca data: %02d-%02d-%d\n", local->tm_mday, local->tm_mon + 1, local->tm_year + 1900); 

    return 0; 
} 
```

Po uruchomieniu powyższego kodu, powinniśmy uzyskać wyjście:

```
Bieżąca data: 27-10-2021
```

## Deep Dive

Funkcje "time()" i "localtime()" wykorzystują wewnętrznie struktury czasu, aby przetworzyć bieżący czas. Struktura ta zawiera wiele pól, takich jak rok, miesiąc, dzień, godzina, minuta, sekunda, itp. Dzięki temu możemy uzyskać różne informacje o bieżącej dacie i godzinie.

Możemy również zmienić strefę czasową, w której chcemy wyświetlić datę, poprzez ustawienie odpowiedniego pola struktury lokalnego czasu "tm_zone". Możemy również zmienić format wyjściowy używając innych funkcji z biblioteki "time.h", takich jak "strftime()".

## Zobacz także

- [Dokumentacja funkcji time() i localtime() w języku C](https://www.cplusplus.com/reference/ctime/)
- [Przewodnik po programowaniu w języku C](https://www.programuj.com/kursy/c/jak-zaczac-programowanie-w-c?gclid=CjwKCAjwiLGGBhAqEiwAgq3q_407TsylY816hDsRXGzif8_Q-tA28Eg0wPsPfAssmasfQjrGlpqVQRoCkkEQAvD_BwE)
- [Książka "Język ANSI C" autorstwa B. W. Kernighan i D. M. Ritchie](https://helion.pl/ksiazki/jezyk-ansi-c-brian-w-kernighan-dennis-m-ritchie,jansc.htm#format/e)