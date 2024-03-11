---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:59:03.742225-07:00
description: "Logowanie w C polega na rejestrowaniu przep\u0142ywu i znacz\u0105cych\
  \ zdarze\u0144 programu podczas jego wykonania, zapewniaj\u0105c namacalny przegl\u0105\
  d jego zachowania i\u2026"
lastmod: '2024-03-11T00:14:09.110322-06:00'
model: gpt-4-0125-preview
summary: "Logowanie w C polega na rejestrowaniu przep\u0142ywu i znacz\u0105cych zdarze\u0144\
  \ programu podczas jego wykonania, zapewniaj\u0105c namacalny przegl\u0105d jego\
  \ zachowania i\u2026"
title: Logowanie
---

{{< edit_this_page >}}

## Co i dlaczego?

Logowanie w C polega na rejestrowaniu przepływu i znaczących zdarzeń programu podczas jego wykonania, zapewniając namacalny przegląd jego zachowania i wydajności. Programiści wykorzystują logowanie do celów debugowania, monitorowania zdrowia oprogramowania i zapewniania bezpieczeństwa systemu.

## Jak to zrobić:

W C logowanie można osiągnąć za pomocą podstawowych operacji na plikach lub korzystając z bardziej zaawansowanych bibliotek. Dla uproszczenia zaczniemy od standardowej biblioteki wejścia/wyjścia. Poniższe fragmenty kodu prezentują podstawowe implementacje logowania.

Aby zalogować proste wiadomości:

```c
#include <stdio.h>

int main() {
    FILE *logFile;
    logFile = fopen("application.log", "a"); // Otwórz plik logu w trybie dopisywania
    
    if (logFile == NULL) {
        perror("Błąd otwarcia pliku logu.");
        return -1;
    }
    
    fprintf(logFile, "Uruchamianie aplikacji.\n");
    
    // Twoja logika aplikacji
    
    fprintf(logFile, "Aplikacja zakończyła się powodzeniem.\n");
    fclose(logFile);
    
    return 0;
}
```

Wynik w `application.log`:

```
Uruchamianie aplikacji.
Aplikacja zakończyła się powodzeniem.
```

Aby uwzględnić bardziej szczegółowe logi z datami i poziomami logowania:

```c
#include <stdio.h>
#include <time.h>

void logMessage(FILE *logFile, const char* level, const char* message) {
    time_t now;
    time(&now);
    char* datetime = ctime(&now);
    datetime[strlen(datetime)-1] = '\0'; // Usuń znak nowej linii
    fprintf(logFile, "[%s] %s - %s\n", datetime, level, message);
}

int main() {
    FILE *logFile;
    logFile = fopen("detailed.log", "a");
    
    if (logFile == NULL) {
        perror("Błąd otwarcia pliku logu.");
        return -1;
    }
    
    logMessage(logFile, "INFO", "Uruchamianie aplikacji");
    // Twoja logika aplikacji
    logMessage(logFile, "ERROR", "Przykładowy błąd");
    
    fclose(logFile);
    
    return 0;
}
```

Wynik w `detailed.log`:

```
[Czw Mar 10 14:32:01 2023] INFO - Uruchamianie aplikacji
[Czw Mar 10 14:32:02 2023] ERROR - Przykładowy błąd
```

## Dokładniejsze spojrzenie

Jak pokazano, logowanie w C opiera się na prostych operacjach na plikach, co jest skuteczne, ale nie tak potężne ani elastyczne jak mechanizmy logowania w innych językach, takich jak moduł `logging` w Pythonie czy `Log4j` w Javie. Dla bardziej zaawansowanych możliwości logowania w C, deweloperzy często zwracają się ku bibliotekom takim jak `syslog` w systemach podobnych do Unix, które zapewniają zarządzanie logami na poziomie systemu, lub do bibliotek firm trzecich, takich jak `log4c`.

Historycznie rzecz biorąc, logowanie było integralną częścią programowania, sięgającą początków praktyk programistycznych, gdzie śledzenie i rozumienie przepływu programów i błędów było przede wszystkim wykonywane poprzez fizyczne wydruki. Wraz z ewolucją systemów logowanie stało się bardziej zaawansowane, wspierając obecnie różne poziomy powagi, rotację logów oraz asynchroniczne logowanie.

Chociaż standardowa biblioteka C dostarcza podstawowych narzędzi do implementacji logowania, jej ograniczenia często prowadzą do tworzenia własnych ram logowania lub adopcji zewnętrznych bibliotek dla bardziej bogatych w funkcje i elastycznych rozwiązań logowania. Pomimo tych ograniczeń, zrozumienie i implementacja podstawowego logowania w C jest kluczowe dla debugowania i utrzymania oprogramowania, szczególnie w środowiskach, gdzie zewnętrzne zależności mają być zminimalizowane.
