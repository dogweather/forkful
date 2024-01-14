---
title:    "C: Obliczanie daty w przyszłości lub przeszłości"
keywords: ["C"]
---

{{< edit_this_page >}}

# Dlaczego
Obliczanie daty w przyszłości lub w przeszłości może być przydatne w wielu sytuacjach, na przykład w tworzeniu kalendarzy lub planowania wydarzeń.

# Jak to zrobić
Aby obliczyć datę w przyszłości lub w przeszłości, możemy wykorzystać funkcję `time()` oraz struktury danych `tm` dostępne w języku C. Najpierw musimy zadeklarować i zainicjalizować zmienną `struct tm`, żeby móc wprowadzić informacje o dacie i czasie, dla którego chcemy obliczyć przyszłą lub przeszłą datę. Następnie przy użyciu funkcji `time()` pobieramy obecny czas i zapisujemy go w innej zmiennej typu `time_t`. Na koniec, przy użyciu funkcji `mktime()` przekształcamy strukturę `tm` na wartość typu `time_t` reprezentującą daną datę. Przykładowy kod w języku C wyglądałby następująco:
```C
#include <stdio.h>
#include <time.h>

int main(void) {
    // Inicjalizacja struktury tm
    struct tm date = {0};

    // Ustawienie daty i czasu, dla którego chcemy obliczyć przyszłą/przeszłą datę
    date.tm_year = 2022 - 1900; // rok - 1900
    date.tm_mon = 6; // miesiąc (od 0 do 11)
    date.tm_mday = 1; // dzień
    date.tm_hour = 13; // godzina (od 0 do 23)
    date.tm_min = 30; // minuta
    date.tm_sec = 0; // sekunda

    // Pobranie obecnego czasu
    time_t now = time(NULL);

    // Obliczenie daty
    time_t new_time = mktime(&date);

    // Wyświetlenie wyniku
    printf("Przyszła/data w przeszłości: %s", ctime(&new_time));
    
    return 0;
}
```
Przykładowy wynik dla powyższego kodu w przypadku obliczania daty w przyszłości:
```
Przyszła/data w przeszłości: Wed Jun 1 13:30:00 2022
```

# Głębsze zanurzenie
Funkcja `mktime()` korzysta z aktualnych ustawień strefy czasowej oraz wprowadzonej daty i czasu, aby obliczyć wartość typu `time_t` dla danej daty. Jednak może się zdarzyć, że chcemy obliczyć datę w innej strefie czasowej. W takim przypadku, możemy użyć funkcji `localtime()` do przekonwertowania wartości zwróconej przez funkcję `time()` na wartość typu `tm`, którą następnie można zmodyfikować. Po zmianie daty i czasu, możemy ponownie użyć funkcji `mktime()`, aby wyliczyć wartość typu `time_t` w wybranej strefie czasowej. Przykładowy kod wyglądałby następująco:
```C
#include <stdio.h>
#include <time.h>

int main(void) {
    // Pobranie obecnego czasu
    time_t now = time(NULL);

    // Przekonwertowanie na strukturę tm
    struct tm *local_now = localtime(&now);

    // Dodanie jednego dnia do daty
    local_now->tm_mday += 1;

    // Wyliczenie daty dla wybranej strefy czasowej
    time_t new_time = mktime(local_now);

    // Wyświetlenie wyniku
    printf("Jutro: %s", ctime(&new_time));

    return 0;
}
```
Przykładowy wynik dla powyższego kodu w przypadku użytkownika mieszającego w strefie czasowej UTC+2:
```
Jutro: Tue Aug 17 21: