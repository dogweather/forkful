---
title:                "Rejestrowanie zdarzeń"
date:                  2024-01-26T01:03:40.767781-07:00
model:                 gpt-4-1106-preview
simple_title:         "Rejestrowanie zdarzeń"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/logging.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Logowanie to w zasadzie notowanie tego, co robi Twoja aplikacja – rodzaj dziennika, ale dla kodu. Programiści używają go, aby śledzić detale, takie jak zmiany stanów, zdarzenia systemowe i uporczywe błędy, upewniając się, że żadne zacięcia nie przejdą niezauważone.

## Jak to zrobić:
W Fish, logowanie może być tak proste, jak przekierowanie standardowego wyjścia i strumienia błędów do pliku. Zróbmy wpis w logu dla czasu rozpoczęcia i zakończenia naszego skryptu.

```fish
function log_start
  echo (date "+%Y-%m-%d %H:%M:%S") " - Skrypt rozpoczęty" >> my_app.log
end

function log_end
  echo (date "+%Y-%m-%d %H:%M:%S") " - Skrypt zakończony" >> my_app.log
end

log_start
# ... zadania twojego skryptu ...
log_end

cat my_app.log
```

Oto, co zobaczysz w `my_app.log`:

```
2023-04-01 10:35:47  - Skrypt rozpoczęty
2023-04-01 10:36:02  - Skrypt zakończony
```

Dla zaawansowanego logowania możesz użyć funkcji z parametrami dla poziomu logowania i wiadomości:

```fish
function log_message --argument message
  switch "$argv[1]"
    case 'INFO' 'WARN' 'ERROR'
      set log_level $argv[1]
    case '*'
      set log_level 'DEBUG'
  end
  set log_msg (string join " " $argv[2..-1])
  echo (date "+%Y-%m-%d %H:%M:%S") "[$log_level]" $log_msg >> my_app.log
end

log_message INFO "To jest wiadomość informacyjna."
log_message ERROR "Coś poszło nie tak!"
```

Przykładowy wynik w `my_app.log` będzie:
```
2023-04-01 10:35:47 [INFO] To jest wiadomość informacyjna.
2023-04-01 10:35:49 [ERROR] Coś poszło nie tak!
```

## W głębi tematu
Historycznie, logowanie w skryptach powłoki było wykonane za pomocą wielu poleceń `echo`, i chociaż jest to nadal opcja, implementacja bardziej złożonych systemów może być wyzwaniem. Fish nie posiada wbudowanego mechanizmu logowania, tak jak niektóre inne języki skryptowe czy programistyczne, więc często trzeba stworzyć własny.

Alternatywy dla wbudowanego polecenia `echo` w Fish dla logowania obejmują narzędzia Unixowe takie jak `syslog` czy `logger`, które łączą się z demonem logów systemowych, oferując bardziej zintegrowane podejście do logowania zdarzeń systemowych.

Prostota Fish pozwala na stworzenie funkcji do obsługi rozbudowanego logowania, ustawiając różne poziomy, które możesz włączać lub wyłączać. Niektóre implementacje mogą nawet zawierać nazwę skryptu, numer linii i znacznik czasu, co ułatwia śledzenie kroków prowadzących do danego zdarzenia.

## Zobacz także
- Dokumentacja Fish Shell o pisaniu funkcji: https://fishshell.com/docs/current/#syntax-function
- Podstawowe wskazówki do skryptowania w powłoce: https://developer.ibm.com/tutorials/l-lpic1-103-4/
- Przewodnik po protokole Syslog: https://tools.ietf.org/html/rfc5424