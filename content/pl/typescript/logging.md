---
title:                "Rejestrowanie zdarzeń"
date:                  2024-01-26T01:08:34.282533-07:00
model:                 gpt-4-1106-preview
simple_title:         "Rejestrowanie zdarzeń"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/logging.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Logowanie to proces zapisywania zdarzeń, błędów i innych istotnych informacji podczas wykonania programu do zewnętrznego medium, często plików lub baz danych. Programiści używają logów do monitorowania zachowania oprogramowania, debugowania problemów oraz śledzenia aktywności systemowych dla analizy bezpieczeństwa i wydajności.

## Jak to zrobić:

W TypeScript można łatwo zaimplementować podstawowe logowanie za pomocą metod konsoli lub zintegrować bardziej zaawansowane logowanie z bibliotekami takimi jak `winston` czy `pino`. Oto podstawowy przykład z użyciem `console.log` i bardziej zaawansowany z `winston`.

```TypeScript
// Podstawowe logowanie do konsoli
console.log('Info: Uruchamianie aplikacji...');
console.error('Błąd: Nie można pobrać danych.');

// Przykładowe wyjście
// Info: Uruchamianie aplikacji...
// Błąd: Nie można pobrać danych.
```

Dla bardziej zaawansowanego logowania, skonfigurujmy `winston`:

```TypeScript
import { createLogger, format, transports } from 'winston';

const logger = createLogger({
  level: 'info',
  format: format.combine(
    format.timestamp({ format: 'YYYY-MM-DD HH:mm:ss' }),
    format.printf(info => `${info.timestamp} ${info.level}: ${info.message}`)
  ),
  transports: [
    new transports.Console(),
    new transports.File({ filename: 'combined.log' })
  ]
});

logger.info('Serwer uruchomiony!');
logger.warn('Ostrzeżenie: Niskie miejsce na dysku.');
logger.error('Nie udało się połączyć z bazą danych.');

// Przykładowe wyjście w pliku combined.log
// 2023-01-20 14:42:07 info: Serwer uruchomiony!
// 2023-01-20 14:42:09 warn: Ostrzeżenie: Niskie miejsce na dysku.
// 2023-01-20 14:42:12 error: Nie udało się połączyć z bazą danych.
```

## Szczegółowa analiza:

Pojęcie logowania w kontekście informatyki sięga wczesnych dni programowania, gdzie termin ten wywodzi się z "dziennika pokładowego", systemu prowadzenia dokumentacji marynarskiej. Historycznie, zdarzenia programu często były logowane do wydruków fizycznych lub wyjść terminalowych, zwłaszcza w erze mainframe'ów.

Przesuwając się do dziś, masz do dyspozycji mnóstwo narzędzi i bibliotek, które spełniają różne potrzeby logowania, od prostych plików tekstowych do skomplikowanych systemów zarządzania logami. Alternatywy dla `winston` to między innymi `pino`, które oferuje wysoką wydajność, oraz `Bunyan`, który jest oparty na JSON-ie. Pracując z Node.js, biblioteki logowania często zapewniają mechanizmy strumieniowania logów do różnych miejsc docelowych, obsługę rotacji logów i konfigurowalne formatery.

Pod względem implementacji, wiadomości logów zwykle zawierają znacznik czasu, poziom ważności (takie jak info, warn, error) i właściwą wiadomość. Dobre praktyki logowania polecają odpowiednie kategoryzowanie poziomów logowania, unikanie wrażliwych danych w logach oraz rozważenie implikacji wydajnościowych w aplikacjach o dużej przepustowości.

## Zobacz również:

- [Winston - Logger właściwie do wszystkiego](https://www.npmjs.com/package/winston)
- [Pino - Bardzo niskonakładowy logger dla Node.js](https://www.npmjs.com/package/pino)
- [Najlepsze praktyki logowania w Node.js](https://thisdavej.com/using-winston-a-versatile-logging-library-for-node-js/)
- [12 Factor App - Logi](https://12factor.net/logs)
