---
title:    "C++: Obliczanie daty w przyszłości lub przeszłości"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego

Obliczanie daty w przyszłości lub przeszłości może być niezbędne w wielu sytuacjach programistycznych. Na przykład, może być użyteczne podczas tworzenia aplikacji związanych z czasem, takich jak kalendarze lub systemy rezerwacji. W tym artykule dowiecie się, jak napisać prosty program w języku C++, który będzie w stanie wyliczyć datę w przyszłości lub przeszłości.

## Jak to zrobić

Aby wyliczyć datę w przyszłości lub przeszłości, będziemy potrzebować kilku podstawowych informacji: aktualnej daty, liczby dni do przeliczenia oraz kierunku (przyszłość lub przeszłość). Wykorzystamy bibliotekę <ctime>, która udostępnia użyteczne funkcje związane z czasem.

Najpierw zadeklarujemy potrzebne zmienne, czyli aktualną datę, liczbę dni oraz kierunek:

```
#include <ctime>
using namespace std;

int main()
{
    time_t currentTime;
    struct tm *localTime;
    int numberOfDays;
    char direction;
}
```

Następnie pobierzemy aktualną datę i przypiszemy ją do zmiennej `currentTime` oraz przeliczymy ją na lokalny czas, który przypiszemy do zmiennej `localTime`:

```
currentTime = time(NULL);
localTime = localtime(&currentTime);
```

Teraz poprosimy użytkownika o podanie liczby dni oraz kierunku (przyszłość lub przeszłość) i przypiszemy je do odpowiednich zmiennych.

Następnie użyjemy funkcji `mktime()` do przeliczenia daty na liczbę sekund oraz funkcji `difftime()` do wyliczenia różnicy w sekundach między aktualną datą a przeliczoną datą:

```
time_t newTime;
if (direction == 'F') {
    newTime = mktime(localTime) + numberOfDays * 24 * 60 * 60;
} else if (direction == 'P') {
    newTime = mktime(localTime) - numberOfDays * 24 * 60 * 60;
}
    
double difference = difftime(newTime, currentTime);
```

Na koniec użyjemy funkcji `ctime()` do zamiany daty z powrotem na czytelną dla człowieka formę oraz wypiszemy ją na ekran:

```
cout << "Nowa data: " << ctime(&newTime);
```

Po uruchomieniu programu i podaniu odpowiednich danych, powinniśmy otrzymać wynik wyliczonej daty w przyszłości lub przeszłości.

## Deep Dive

W powyższym przykładzie wykorzystaliśmy funkcje `mktime()` i `difftime()`, które są bardzo przydatne podczas obliczania dnia w przyszłości lub przeszłości. Jednak istnieje wiele innych metod, które mogą zostać wykorzystane do tego celu, takich jak biblioteka <chrono> albo funkcje systemowe dostarczane przez konkretny system operacyjny.

Ważne jest także prawidłowe wykorzystanie i przetwarzanie dat w różnych strefach czasowych oraz uwzględnienie zmiany czasu letniego i zwykłego.

Jeśli chcesz dowiedzieć się więcej o obliczaniu daty w przyszłości lub przeszłości, polecamy zapoznać się z dokumentacją biblioteki <ctime> oraz poszukać innych przydatnych funkcji i metod związanych z czasem.

## Zobacz też

- https://www.cplusplus.com/reference/ctime/
- https://www.cplusplus.com/reference/chrono/
- https://www.geeksforgeeks.org/working-with-date-and-time-in-c-chrono-library/