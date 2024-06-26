---
date: 2024-01-26 01:07:59.699207-07:00
description: "Jak to zrobi\u0107: Ruby ma wbudowany modu\u0142 do logowania, `Logger`,\
  \ kt\xF3ry jest bardzo \u0142atwy w u\u017Cyciu. Oto kr\xF3tki przyk\u0142ad, kt\xF3\
  ry pomo\u017Ce Ci zacz\u0105\u0107."
lastmod: '2024-03-13T22:44:35.938723-06:00'
model: gpt-4-1106-preview
summary: "Ruby ma wbudowany modu\u0142 do logowania, `Logger`, kt\xF3ry jest bardzo\
  \ \u0142atwy w u\u017Cyciu."
title: "Rejestrowanie zdarze\u0144"
weight: 17
---

## Jak to zrobić:
Ruby ma wbudowany moduł do logowania, `Logger`, który jest bardzo łatwy w użyciu. Oto krótki przykład, który pomoże Ci zacząć:

```ruby
require 'logger'

# Stworzenie Loggera, który wypisuje informacje na STDOUT
logger = Logger.new(STDOUT)
logger.level = Logger::INFO

# Przykładowe komunikaty logów
logger.info("To jest komunikat informacyjny")
logger.warn("To jest komunikat ostrzegawczy")
logger.error("To jest komunikat o błędzie")
```

Uruchomienie powyższego skryptu wyświetli coś takiego:

```
I, [2023-03-15T10:00:00.123456 #1234]  INFO -- : To jest komunikat informacyjny
W, [2023-03-15T10:00:01.234567 #1234]  WARN -- : To jest komunikat ostrzegawczy
E, [2023-03-15T10:00:02.345678 #1234] ERROR -- : To jest komunikat o błędzie
```

Możesz skonfigurować format logu i poziom, aby odfiltrować niepotrzebny szum, a także przekierować logi do różnych wyjść, takich jak plik czy nawet zewnętrzna usługa logowania.

## Pogłębiona analiza
Logowanie jest jak długa tradycja w programowaniu. Historycznie logi były prostymi plikami tekstowymi, przeszukiwanymi ręcznie za pomocą narzędzi takich jak `grep`. Ale koncepcja ta rozwinęła się w cały ekosystem zaawansowanych frameworków i usług logowania, takich jak Log4j, Syslog na Linuxie, czy Sematext i Loggly w erze chmurowej.

`Logger` Ruby'ego to nieskomplikowany sposób, aby zacząć, ale jeśli potrzebujesz większej mocy i elastyczności, możesz sprawdzić alternatywy takie jak Lograge lub Semantic Logger. Te biblioteki dobrze współpracują z aplikacjami Ruby'ego, oferując bardziej szczegółową kontrolę nad formatowaniem logów, w tym logi strukturalne (format JSON), lepszą wydajność i bezproblemową integrację z innymi usługami.

Każda biblioteka logowania Ruby'ego ma własny sposób działania, ale w gruncie rzeczy wszystkie opierają się na idei instancji loggera, do której wysyłasz komunikaty. Logger obsługuje te komunikaty na podstawie ustawionych poziomów — DEBUG, INFO, WARN, ERROR, FATAL i UNKNOWN — i decyduje, co z nimi zrobić: wydrukować je, zapisać do pliku, przesłać przez sieć itp.

## Zobacz również
Aby dokładniej przyjrzeć się wbudowanemu modułowi logowania Ruby'ego, sprawdź oficjalną dokumentację:

Jeśli interesują Cię bardziej zaawansowane możliwości logowania lub chcesz zbadać zewnętrzne gemy:
- [Lograge](https://github.com/roidrage/lograge)

Jeśli chodzi o ogólne praktyki i filozofię logowania (niekoniecznie specyficzne dla Ruby), te artykuły są lekturą na wieki:
- [Książka Google's Site Reliability Engineering - Rozdział 16: Radzenie sobie z przeciążeniem](https://sre.google/sre-book/handling-overload/#log-messages)
- [12 Factor App - Logi](https://12factor.net/logs)
