---
title:                "Pisanie do standardowego błędu"
date:                  2024-01-19
html_title:           "Arduino: Pisanie do standardowego błędu"
simple_title:         "Pisanie do standardowego błędu"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
W PHP piszemy do standardowego błędu (`stderr`), by oddzielić normalne dane wyjściowe od informacji o błędach i logów. Pomaga to w lepszym zarządzaniu i monitoringu aplikacji.

## Jak to zrobić:
W PHP do `stderr` możemy pisać prosto przy użyciu `fopen('php://stderr', 'w')` albo `fwrite(STDERR, ...)`. Oto przykłady:

```php
<?php
// Otworzenie stderr i zapis do niego
$fileHandle = fopen('php://stderr', 'w');
fwrite($fileHandle, "To jest błąd\n");
fclose($fileHandle);

// Alternatywnie, korzystanie ze stałej STDERR
fwrite(STDERR, "To jest błąd\n");
?>
```
Po uruchomieniu, zobaczysz na ekranie "To jest błąd", ale nie będzie to wysłane do standardowego wyjścia `stdout`, a do `stderr`.

## Deep Dive:
W przeszłości, przed PHP 4.3.0, nie było bezpośredniego dostępu do `stderr`. Deweloperzy musieli szukać obejść, takich jak wywołanie zewnętrznych komend systemowych. Alternatywą dla `stderr` jest logowanie do plików lub usług monitorujących. Przy zapisie do `stderr`, PHP używa buforowania, więc pamiętaj o zamykaniu uchwytu pliku, aby uniknąć utraty danych.

## Zobacz również:
- [Sekcja stderr w manualu GNU](https://www.gnu.org/software/libc/manual/html_node/Standard-Streams.html)
