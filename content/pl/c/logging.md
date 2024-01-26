---
title:                "Rejestrowanie zdarzeń"
date:                  2024-01-26T01:00:20.449567-07:00
model:                 gpt-4-1106-preview
simple_title:         "Rejestrowanie zdarzeń"
programming_language: "C"
category:             "C"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/logging.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Logowanie jest zasadniczo rejestrowaniem tego, co robi Twój program, zwykle poprzez zapisywanie komunikatów do pliku lub terminala. Programiści robią to, aby śledzić wydarzenia, diagnozować problemy oraz mieć ślad audytowy, który opowiada historię działania aplikacji na przestrzeni czasu.

## Jak to zrobić:
Zacznijmy od podstaw. C nie ma wbudowanego frameworka do logowania, ale możesz stworzyć coś prostego z użyciem `stdio.h`. Oto jak:

```c
#include <stdio.h>
#include <time.h>

void logMessage(const char* message) {
    time_t now;
    time(&now);
    char *data = ctime(&now);
    data[strlen(data) - 1] = '\0'; // Usuń znak nowej linii na końcu wyniku ctime()
    printf("[%s] %s\n", data, message);
}

int main() {
    logMessage("Aplikacja została uruchomiona.");
    // ... tutaj wpisz swój kod ...
    logMessage("Aplikacja wykonuje coś ważnego.");
    // ... kontynuacja kodu ...
    logMessage("Aplikacja zakończyła działanie.");
    return 0;
}
```

Przykładowe wyjście może wyglądać tak:

```
[Wto Mar 9 12:00:01 2023] Aplikacja została uruchomiona.
[Wto Mar 9 12:00:02 2023] Aplikacja wykonuje coś ważnego.
[Wto Mar 9 12:00:03 2023] Aplikacja zakończyła działanie.
```

Oczywiście, w rzeczywistości prawdopodobnie chciałbyś zapisać komunikaty do pliku zamiast do terminala, obsługiwać różne poziomy logów i może użyć gotowej biblioteki.

## Dalsze kroki
Logowanie w C ma swój swoisty urok – jest równie niskopoziomowe, jak większość reszty języka. Historycznie logowanie było wykonywane przy użyciu `fprintf` z `stderr` lub wskaźnika pliku. W miarę komplikowania się programów rosły również potrzeby logowania, co doprowadziło do rozwoju bibliotek takich jak `syslog` w systemach Unix, które mogły obsługiwać logowanie z wielu źródeł o różnym stopniu ważności.

We współczesnym krajobrazie jest wiele bibliotek logowania C, takich jak `zlog`, `log4c` i `glog`, które oferują bogaty zestaw funkcji, w tym rotację logów, strukturalne logowanie i wielowątkowe logowanie. Te rozwiązania pozwalają na dokładną kontrolę nad werbalnością logów, miejscami docelowymi i formatami.

Podczas implementacji systemu logowania szczegóły takie jak formatowanie znaczników czasu, zarządzanie plikami logów i wydajność wymagają przemyślenia. Znaczniki czasu w logach są kluczowe dla korelacji zdarzeń, podczas gdy rotacja logów zapewnia, że pliki dziennika nie zajmują zbyt wiele miejsca na dysku. Działanie logowania powinno być również szybkie i nieblokujące dla głównego przepływu aplikacji, aby logowanie nie stało się wąskim gardłem.

## Zobacz również
Żeby zagłębić się w biblioteki i praktyki logowania w C, sprawdź te zasoby:

- Podręcznik GNU `syslog`: https://www.gnu.org/software/libc/manual/html_node/Syslog.html
- `zlog`: Wysoko konfigurowalna biblioteka do logowania dla C - https://github.com/HardySimpson/zlog
- `log4c`: Framework logowania dla C wzorowany na Log4j - http://log4c.sourceforge.net/
- `glog`: Biblioteka logowania na poziomie aplikacji od Google - https://github.com/google/glog