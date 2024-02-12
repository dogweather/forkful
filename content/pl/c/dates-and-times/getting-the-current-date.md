---
title:                "Pobieranie bieżącej daty"
date:                  2024-02-03T17:57:23.451971-07:00
model:                 gpt-4-0125-preview
simple_title:         "Pobieranie bieżącej daty"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?

Pobieranie bieżącej daty w języku C wiąże się z wykorzystaniem standardowej biblioteki C do pobrania i sformatowania bieżącej daty i czasu systemu. Programiści często potrzebują tej funkcjonalności do logowania, znakowania czasowego lub planowania funkcji w swoich aplikacjach.

## Jak to zrobić:

W C, nagłówek `<time.h>` zapewnia niezbędne funkcje i typy do pracy z datami i czasem. Funkcja `time()` pobiera bieżący czas, podczas gdy `localtime()` konwertuje ten czas na strefę czasową lokalną. Aby wyświetlić datę, używamy `strftime()`, aby sformatować ją jako ciąg znaków.

Oto podstawowy przykład:

```c
#include <stdio.h>
#include <time.h>

int main() {
    char buffer[80];
    time_t rawtime;
    struct tm *timeinfo;

    // Pobranie bieżącego czasu
    time(&rawtime);
    // Konwersja na czas lokalny
    timeinfo = localtime(&rawtime);
    
    // Formatowanie daty i wyświetlanie jej
    strftime(buffer, 80, "Dzisiejsza data to %Y-%m-%d", timeinfo);
    printf("%s\n", buffer);

    return 0;
}
```

Przykładowe wyjście może wyglądać tak:

```
Dzisiejsza data to 2023-04-12
```

## Zagłębienie się

Obsługa czasu w C, ułatwiona przez `<time.h>`, nawiązuje do najwcześniejszych dni języka i systemów UNIX. Opiera się na typie danych `time_t`, który reprezentuje bieżący czas jako liczbę sekund od ery Unix (1 stycznia 1970). Chociaż jest to efektywne i uniwersalnie kompatybilne, oznacza to również, że funkcje czasu standardowej biblioteki C są z natury ograniczone przez zakres i rozdzielczość `time_t`.

Współczesne aplikacje, szczególnie te wymagające precyzyjnych znaczników czasowych lub mające do czynienia z datami daleko w przyszłości lub przeszłości, mogą uznać te ograniczenia za wyzwanie. Na przykład, problem roku 2038 jest znaną ilustracją, gdzie systemy używające 32-bitowego `time_t` będą miały przepełnienie.

Dla bardziej złożonej obsługi czasu i daty, wielu programistów zwraca się do zewnętrznych bibliotek lub funkcjonalności dostarczonych przez system operacyjny. Na przykład, w C++, biblioteka `<chrono>` oferuje bardziej precyzyjne i wszechstronne możliwości manipulacji czasem.

Pomimo swoich ograniczeń, prostota i wszechobecność funkcji czasowych C sprawiają, że są one doskonale odpowiednie dla wielu aplikacji. Zrozumienie tych narzędzi jest fundamentalne dla programistów C, oferując mieszankę historycznego kontekstu programowania i praktycznej, codziennej użyteczności.
