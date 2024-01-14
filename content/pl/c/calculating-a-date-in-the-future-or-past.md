---
title:    "C: Obliczanie daty w przyszłości lub przeszłości"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Dlaczego

Obliczanie daty w przyszłości lub przeszłości może być bardzo przydatne w wielu sytuacjach. Na przykład, jeśli musisz zaplanować termin spotkania lub wydarzenia, lub sprawdzić, kiedy upłynie okres ważności dokumentu. Dzięki temu zabiegowi możesz również lepiej zrozumieć, jak działają daty w komputerowych systemach i jak manipulować nimi w swoim kodzie.

# Jak to zrobić

Kodowanie obliczeń dat może wydawać się skomplikowane, ale w rzeczywistości jest to dosyć proste. W poniższym przykładzie użyjemy języka C do obliczenia daty w przeszłości, a następnie w przyszłości.

```C
#include <stdio.h>
#include <time.h>
int main()
{
  // obliczanie daty 30 dni w przeszłości
  time_t currentTime;
  struct tm *dateInfo;
  char dateInPast[15];
  currentTime = time(NULL);
  dateInfo = localtime(&currentTime);
  time_t pastTime = currentTime - (30 * 24 * 60 * 60); // 30 dni w sekundach
  dateInfo = localtime(&pastTime);
  strftime(dateInPast, sizeof(dateInPast), "%d-%m-%Y", dateInfo);
  printf("Data 30 dni w przeszłości: %s", dateInPast);
  // obliczanie daty 30 dni w przyszłości
  char dateInFuture[15];
  time_t futureTime = currentTime + (30 * 24 * 60 * 60); // 30 dni w sekundach
  dateInfo = localtime(&futureTime);
  strftime(dateInFuture, sizeof(dateInFuture), "%d-%m-%Y", dateInfo);
  printf("Data 30 dni w przyszłości: %s", dateInFuture);
  return 0;
}
```
Przy użyciu funkcji `localtime` i `strftime` możemy pobierać informacje o bieżącym czasie oraz formatować je według naszych preferencji. W powyższym przykładzie używamy formatu `%d-%m-%Y`, co oznacza, że data będzie wyświetlana w formacie dzień-miesiąc-rok. W celu obliczenia daty w przyszłości lub przeszłości, musimy dodać (lub odjąć) odpowiednią liczbę sekund od bieżącego czasu, co wykonał kod `currentTime + (30 * 24 * 60 * 60)`.

# Deep Dive

Obliczanie dat w przyszłości lub przeszłości polega na manipulowaniu różnymi jednostkami czasu. W naszym przykładzie użyliśmy 30 dni, ale możemy również używać innych jednostek, takich jak godziny, minuty czy nawet milisekundy. Pobierając bieżący czas i dodając lub odejmując odpowiednią liczbę sekund, możemy uzyskać pożądany efekt.

W języku C istnieje wiele funkcji pomocniczych, które ułatwiają manipulowanie datami. Oprócz `localtime` i `strftime`, warto zapoznać się również z funkcjami `mktime`, `gmtime` oraz `difftime`, które mogą być przydatne w bardziej zaawansowanych scenariuszach.

# Zobacz również

- [Przykłady obliczania dat](https://www.programiz.com/c-programming/examples/current-time)
- [Dokumentacja funkcji czasu w języku C](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [Manipulacja datami w języku C](https://www.geeksforgeeks.org/date-manipulation-in-c-c/)