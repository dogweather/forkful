---
title:    "C: Konwertowanie daty na ciąg znaków"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Istnieje wiele powodów, dla których może być potrzebne skonwertowanie daty na łańcuch znaków w języku C. Może to być konieczne do wyświetlenia daty w czytelny sposób dla użytkownika lub do przetwarzania danych w systemach zewnętrznych, które wymagają daty w postaci łańcucha znaków. Bez względu na powód, wiedza na temat tego procesu jest niezbędna dla każdego programisty C.

## Jak to zrobić

Konwersja daty na łańcuch znaków może się odbyć na różne sposoby, w zależności od preferencji programisty i dostępnych funkcji. Poniżej przedstawiono przykładowe kody w języku C, które ilustrują sposób wykorzystania kilku popularnych funkcji do przeprowadzenia tego procesu.

```C
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
  
int main()
{
    // Pobranie aktualnej daty i czasu
    time_t t = time(NULL);

    // Utworzenie struktury tm na podstawie aktualnej daty
    struct tm *now = localtime(&t);

    // Utworzenie łańcucha znaków o odpowiedniej wielkości
    char str_date[11];

    // Użytkowanie funkcji strftime do skonwertowania daty na łańcuch znaków
    strftime(str_date, sizeof(str_date), "%d-%m-%Y", now);

    // Wyświetlenie łańcucha znaków w konsoli
    printf("Aktualna data: %s", str_date);

    return 0;
}
```

W powyższym przykładzie użyliśmy funkcji `strftime` do skonwertowania daty na łańcuch znaków o formacie `dd-mm-yyyy`. Funkcja ta przyjmuje trzy argumenty: łańcuch znaków, zawierający format wyjściowy, strukturę `tm` z aktualną datą oraz maksymalną wielkość łańcucha wynikowego. Dzięki temu metoda jest bezpieczna i minimalizuje możliwość błędów.

## Głębszy zanurzenie

W przypadku, gdy potrzebujemy bardziej zaawansowanej i elastycznej operacji konwertowania daty na łańcuch znaków, w języku C możemy wykorzystać bibliotekę `strftime`. Dzięki niej mamy dostęp do szerokiego zakresu formatów daty oraz możliwości ustawiania języka wyjściowego na podstawie lokalizacji.

Ponadto, część systemów operacyjnych udostępnia również specjalne funkcje, takie jak `localtime_s` i `gmtime_s` do bezpiecznej pracy z datami w języku C.

Zapoznanie się z dokładną dokumentacją tych funkcji i biblioteki jest niezbędne, aby w pełni wykorzystać możliwości konwertowania daty na łańcuch znaków w języku C.

## Zobacz także

- [Dokumentacja strftime](https://www.cplusplus.com/reference/ctime/strftime/)
- [Porównanie funkcji localtime i gmtime](https://www.tutorialspoint.com/compare-c-functions-localtime-and-gmtime)
- [Przykłady użycia biblioteki strftime](https://www.geeksforgeeks.org/date-time-library-functions-c-2/)