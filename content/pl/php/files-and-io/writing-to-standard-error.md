---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:06.222548-07:00
description: "Pisanie do standardowego b\u0142\u0119du (stderr) w PHP polega na kierowaniu\
  \ komunikat\xF3w b\u0142\u0119d\xF3w lub diagnozy oddzielnie od standardowego wyj\u015B\
  cia (stdout), co\u2026"
lastmod: '2024-03-11T00:14:08.700525-06:00'
model: gpt-4-0125-preview
summary: "Pisanie do standardowego b\u0142\u0119du (stderr) w PHP polega na kierowaniu\
  \ komunikat\xF3w b\u0142\u0119d\xF3w lub diagnozy oddzielnie od standardowego wyj\u015B\
  cia (stdout), co\u2026"
title: "Pisanie do standardowego b\u0142\u0119du"
---

{{< edit_this_page >}}

## Co i dlaczego?

Pisanie do standardowego błędu (stderr) w PHP polega na kierowaniu komunikatów błędów lub diagnozy oddzielnie od standardowego wyjścia (stdout), co pozwala programistom lepiej zarządzać swoimi strumieniami wyjściowymi dla celów debugowania i logowania. Programiści wykorzystują tę technikę, aby zapewnić, że komunikaty o błędach nie zakłócają wyjścia programu, ułatwiając monitorowanie i rozwiązywanie problemów z aplikacjami.

## Jak to zrobić:

W PHP, pisanie do stderr można osiągnąć za pomocą funkcji `fwrite()` wraz z predefiniowaną stałą `STDERR`, która reprezentuje strumień wyjścia błędów.

```php
<?php
// Pisanie prostego komunikatu do stderr.
fwrite(STDERR, "To jest komunikat błędu.\n");
```

Przykładowe wyjście po wykonaniu skryptu z linii komend:
```
To jest komunikat błędu.
```

Aby zademonstrować bardziej praktyczne użycie, rozważ scenariusz, w którym przetwarzasz dane użytkownika i napotykasz nieoczekiwane dane:
```php
<?php
$input = 'nieoczekiwane dane';

// Symulowanie błędu podczas przetwarzania danych użytkownika.
if ($input === 'nieoczekiwane dane') {
    fwrite(STDERR, "Błąd: Otrzymano nieoczekiwane dane wejściowe.\n");
    exit(1); // Zakończenie z wartością niezerową w celu wskazania błędu.
}
```

Chociaż wbudowane możliwości PHP do obsługi stderr są generalnie wystarczające, w przypadku trudniejszych aplikacji lub chęci integracji logowania stderr z zewnętrznymi systemami, biblioteki stron trzecich takie jak Monolog mogą być potężnym sojusznikiem. Monolog to biblioteka do logowania, która może obsługiwać stderr wśród wielu innych celów (pliki, gniazda itp.).

Użycie Monologa do pisania do stderr:

Najpierw upewnij się, że Monolog jest zainstalowany za pomocą Composera:
```
composer require monolog/monolog
```

Następnie możesz skonfigurować Monolog do użycia `StreamHandler` skierowanego na `php://stderr`:

```php
<?php
require 'vendor/autoload.php';

use Monolog\Logger;
use Monolog\Handler\StreamHandler;

// Tworzenie kanału logowania
$log = new Logger('nazwa');
$log->pushHandler(new StreamHandler('php://stderr', Logger::WARNING));

// Dodawanie komunikatu logowania do stderr
$log->warning('To jest komunikat ostrzegawczy.');
```

Powyższy kod wykorzystuje Monolog do wysłania komunikatu ostrzegawczego do stderr, co jest szczególnie przydatne dla aplikacji, które wymagają szczegółowej konfiguracji logowania lub zewnętrznego monitorowania logów.
