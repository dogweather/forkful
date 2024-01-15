---
title:                "Pobieranie aktualnej daty"
html_title:           "C: Pobieranie aktualnej daty"
simple_title:         "Pobieranie aktualnej daty"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach niezwykle często korzystamy z aktualnego czasu i daty w naszych programach. Mogą one służyć do wyświetlania informacji o ostatnim logowaniu użytkownika, aktualizacji plików czy generowania raportów z danych zapisanych w konkretnym dniu. W tym artykule dowiesz się, jak w łatwy i szybki sposób uzyskać aktualną datę w języku C.

## Jak to zrobić

Aby uzyskać aktualną datę w języku C, musimy skorzystać z funkcji biblioteki standardowej `time.h`.

```C
#include <stdio.h>
#include <time.h>

int main() {
    time_t current_time;
    struct tm *local_time;

    // Pobranie aktualnego czasu
    current_time = time(NULL);

    // "Rozbicie" czasu na poszczególne jednostki
    local_time = localtime(&current_time);

    // Wyświetlenie aktualnej daty używając odpowiedniego formatu
    printf("Aktualna data: %02d/%02d/%d\n",
            local_time->tm_mday,
            local_time->tm_mon + 1,
            local_time->tm_year + 1900);

    return 0;
}

```

Po uruchomieniu powyższego kodu, powinniśmy zobaczyć na ekranie aktualną datę w formacie `DD/MM/RRRR`, na przykład `17/11/2020`.

## Deep Dive

Funkcja `time()` zwraca liczbę sekund od 1 stycznia 1970 roku, również znana jako "czas EPOKI" lub "czas UNIX". Ta liczba jest w postaci typu danych `time_t`, który jest zdefiniowany w bibliotece `time.h`. Oznacza to, że nie musimy sami wyliczać aktualnego czasu, a jedynie pobieramy go z systemu.

Następnie, wykorzystując funkcję `localtime()`, przetwarzamy liczbę sekund na strukturę `tm` zawierającą informacje o aktualnej dacie i godzinie. Przez użycie odpowiednich funkcji możemy wybrać, które jednostki czasu są dla nas interesujące (np. dzień, miesiąc czy rok).

Ostatnim krokiem jest wyświetlenie daty w pożądanym przez nas formacie. W przykładzie użyliśmy `printf()` z odpowiednim układem znaków, jednak istnieje wiele innych funkcji z biblioteki `time.h` do formatowania daty.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej na temat pracy z datami w języku C, możesz zapoznać się z poniższymi źródłami:

- [Dokumentacja funkcji time() w języku C](https://en.cppreference.com/w/c/chrono/time)
- [Artykuł na temat pracy z datami w języku C na portalu Medium](https://medium.com/@ankurlearnings/dates-and-time-in-c-a3d0565a4abd)
- [Wprowadzenie do pracy z datami w języku C na stronie LearnC.org](https://www.learn-c.org/en/Dates_and_Time)