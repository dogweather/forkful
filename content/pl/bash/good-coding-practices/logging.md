---
title:                "Rejestrowanie zdarzeń"
aliases:
- /pl/bash/logging.md
date:                  2024-01-26T01:00:15.712620-07:00
model:                 gpt-4-1106-preview
simple_title:         "Rejestrowanie zdarzeń"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/logging.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Logowanie to praktyka rejestrowania zdarzeń, błędów i innych istotnych informacji z działających procesów programu do pliku lub strumienia wyjściowego. Programiści wykonują je, aby śledzić zachowanie swoich aplikacji, debugować problemy oraz utrzymywać historyczny zapis operacji, który może wspomagać przyszłe rozwiązywanie problemów.

## Jak to zrobić:

W Bashu, logowanie może być tak proste, jak przekierowanie lub dodawanie wyjścia do pliku. Oto podstawowy przykład:

```Bash
echo "Rozpoczęcie skryptu..." >> script.log
# Twoje polecenia skryptu tutaj
echo "Skrypt zakończony w dniu $(date)" >> script.log
```

Do bardziej zaawansowanych rzeczy, możesz włączyć syslog dla logowania na poziomie systemowym:

```Bash
logger "Własna wiadomość z mojego skryptu"
```

Polecenie `logger` wysyła wiadomość logu do usługi syslog, która następnie obsługuje ją zgodnie z konfiguracją sysloga w systemie.

Przykładowy zapis przechwycony w `script.log`:

```Bash
Rozpoczęcie skryptu...
Skrypt zakończony w dniu Wto Mar 23 09:26:35 PDT 2021
```

## Dogłębna analiza

Historycznie w systemach podobnych do Unix, logowanie było ułatwiane przez usługę syslog, pozwalającej różnym aplikacjom i częściom systemu na centralne logowanie wiadomości. Pozwala to na implementację ustandaryzowanego mechanizmu logowania na przestrzeni całego systemu.

Jeśli chodzi o alternatywy, niektórzy mogą rozważyć użycie `syslog-ng` lub `rsyslog` dla bardziej zaawansowanych funkcji logowania, lub zapisywanie logów do bazy danych szeregów czasowych w celach analitycznych. Dla aplikacji o wyższym poziomie złożoności, używanie dedykowanej biblioteki lub aplikacji do logowania, takich jak Log4j (w ekosystemie Java) lub Monolog (w PHP), które mogą zapewniać strukturalne i konfigurowalne opcje logowania, mogłoby mieć sens nawet dla języka skryptowego, jakim jest Bash.

Sposób implementacji logowania zależy w dużym stopniu od wymagań twojej aplikacji. Jeśli potrzebujesz tylko prostego wyjścia, aby śledzić postęp skryptu, dodanie wiadomości do pliku jest łatwe i wygodne. Jednakże, dla bardziej skalowalnego i solidnego logowania, będziesz chciał zintegrować się z systemem logowania, który wspiera funkcje takie jak rotacja logów, poziomy logowania i zdalne logowanie.

## Zobacz także

- Strony `man` dla funkcji `logger` i `syslog` są zawsze pomocne, spróbuj `man logger` lub `man syslog`.
- Aby uzyskać dogłębne spojrzenie na system logowania, rozważ przeczytanie dokumentacji `rsyslog` i `syslog-ng`.
- Aby dowiedzieć się więcej o historycznym kontekście i zasadach logowania w systemach podobnych do Unix, protokół `Syslog` udokumentowany w RFC 5424 dostarcza wszechstronnych informacji.
