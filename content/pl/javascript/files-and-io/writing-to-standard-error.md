---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:47.544593-07:00
description: "Pisanie do standardowego b\u0142\u0119du (stderr) w JavaScript polega\
  \ na kierowaniu komunikat\xF3w o b\u0142\u0119dach lub jakichkolwiek wa\u017Cnych\
  \ informacji do specyficznego,\u2026"
lastmod: '2024-02-25T18:49:34.186375-07:00'
model: gpt-4-0125-preview
summary: "Pisanie do standardowego b\u0142\u0119du (stderr) w JavaScript polega na\
  \ kierowaniu komunikat\xF3w o b\u0142\u0119dach lub jakichkolwiek wa\u017Cnych informacji\
  \ do specyficznego,\u2026"
title: "Pisanie do standardowego b\u0142\u0119du"
---

{{< edit_this_page >}}

## Co i dlaczego?
Pisanie do standardowego błędu (stderr) w JavaScript polega na kierowaniu komunikatów o błędach lub jakichkolwiek ważnych informacji do specyficznego, oddzielnego strumienia, co jest szczególnie przydatne w środowiskach podobnych do Unix w celach logowania i debugowania. Programiści robią to, aby odróżnić normalne wyjście programu od komunikatów o błędach, co pozwala na czystsze zarządzanie wyjściem i łatwiejsze monitorowanie błędów.

## Jak:
W Node.js, pisanie do stderr można zrealizować za pomocą metody `console.error()` lub poprzez bezpośrednie pisanie do `process.stderr`. Oto przykłady demonstrujące oba podejścia:

```javascript
// Używając console.error()
console.error('To jest komunikat błędu.');

// Bezpośrednio pisząc do process.stderr
process.stderr.write('To jest kolejny komunikat błędu.\n');
```

Przykładowe wyjście dla obu metod pojawiłoby się w strumieniu stderr, nie mieszając się z stdout:
```
To jest komunikat błędu.
To jest kolejny komunikat błędu.
```

Dla bardziej zaawansowanego lub specyficznego dla aplikacji logowania, wielu programistów JavaScript korzysta z bibliotek stron trzecich, takich jak `winston` czy `bunyan`. Oto szybki przykład użycia `winston`:

Najpierw zainstaluj `winston` za pomocą npm:
```shell
npm install winston
```

Następnie skonfiguruj `winston` tak, aby logował błędy do stderr:
```javascript
const winston = require('winston');

const logger = winston.createLogger({
  levels: winston.config.syslog.levels,
  transports: [
    new winston.transports.Console({
      stderrLevels: ['error']
    })
  ]
});

// Logowanie komunikatu o błędzie
logger.error('Błąd zalogowany przez winston.');
```

Ta konfiguracja zapewnia, że gdy zalogujesz błąd za pomocą `winston`, zostanie on skierowany do stderr, co pomaga utrzymać wyraźne rozdzielenie między standardowym wyjściem a wyjściem błędów.
