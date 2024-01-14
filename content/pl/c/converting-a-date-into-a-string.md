---
title:    "C: Konwertowanie daty na ciąg znaków"
keywords: ["C"]
---

{{< edit_this_page >}}

## Dlaczego

Konwertowanie daty na ciąg znaków jest jednym z podstawowych zadań w programowaniu, zwłaszcza w języku C. Wiele programów wymaga wyświetlania daty w formie tekstowej, więc umiejętność dokonania takiej konwersji jest niezbędna dla wielu programistów.

## Jak to zrobić

W języku C istnieje kilka możliwych sposobów na konwertowanie daty na ciąg znaków, ale najbardziej popularne i najprostsze rozwiązanie to użycie funkcji `strftime()` z biblioteki `time.h`.

```C
#include <stdio.h>
#include <time.h>

int main() {
    // Pobranie aktualnej daty i czasu
    time_t mytime = time(NULL);

    // Utworzenie bufora na ciąg znaków
    char buffer[80];

    // Ustawienie formatu daty
    strftime(buffer, 80, "%Y-%m-%d %H:%M:%S", localtime(&mytime));

    // Wyświetlenie skonwertowanego ciągu znaków
    printf("Aktualna data i czas: %s\n", buffer);

    return 0;
}
```

Output:
```
Aktualna data i czas: 2021-10-15 20:37:25
```

W powyższym przykładzie, funkcja `strftime()` przyjmuje trzy argumenty: bufor na ciąg znaków, maksymalną długość bufora oraz format daty. Możemy dostosować format do swoich potrzeb korzystając z różnych wyrażeń, takich jak `%Y` dla roku, `%m` dla miesiąca, `%d` dla dnia czy `%H` dla godziny.

Warto również wspomnieć, że funkcję `strftime()` można wykorzystać do konwertowania innych danych, takich jak temperatura czy napięcie, na wartości tekstowe.

## Deep Dive

Podczas dokonywania konwersji daty na ciąg znaków, ważne jest, aby pamiętać o ustawieniu odpowiedniego języka oraz strefy czasowej. W języku C, możemy to zrobić przy użyciu funkcji `setlocale()` oraz `tzset()`.

Ponadto, warto również wiedzieć, że w niektórych przypadkach możemy spotkać się z różnymi formatami daty, w zależności od systemu operacyjnego czy ustawień lokalnych. Dlatego też, zawsze warto sprawdzić różne wyrażenia formatujące, aby uzyskać odpowiedni wynik.

## Zobacz również

- [Dokumentacja funkcji strftime()](https://www.cplusplus.com/reference/ctime/strftime/)
- [Przykłady użycia funkcji strftime()](https://www.tutorialspoint.com/c_standard_library/c_function_strftime.htm)
- [Poradnik z różnymi formatami daty](https://www.epochconverter.com/programming/c)
- [Inne sposoby konwertowania daty na ciąg znaków w C](https://www.educative.io/edpresso/how-to-convert-a-date-to-a-string-in-c)