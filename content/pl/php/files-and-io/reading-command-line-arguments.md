---
date: 2024-01-20 17:56:28.647491-07:00
description: "Jak to zrobi\u0107: Aby odczyta\u0107 argumenty linii polece\u0144 w\
  \ PHP, korzystamy ze zmiennej globalnej `$argv`, kt\xF3ra zawiera tablic\u0119 argument\xF3\
  w przekazanych do\u2026"
lastmod: '2024-03-13T22:44:35.512902-06:00'
model: gpt-4-1106-preview
summary: "Aby odczyta\u0107 argumenty linii polece\u0144 w PHP, korzystamy ze zmiennej\
  \ globalnej `$argv`, kt\xF3ra zawiera tablic\u0119 argument\xF3w przekazanych do\
  \ skryptu."
title: "Odczytywanie argument\xF3w linii polece\u0144"
weight: 23
---

## Jak to zrobić:
Aby odczytać argumenty linii poleceń w PHP, korzystamy ze zmiennej globalnej `$argv`, która zawiera tablicę argumentów przekazanych do skryptu. Oto prosty przykład:

```php
<?php
// Sprawdzenie, czy istnieją jakieś argumenty
if ($argc > 1) {
    echo "Witaj, " . $argv[1] . "!\n";
} else {
    echo "Hej! Nie podałeś imienia.\n";
}
?>
```

Jeśli uruchomisz ten skrypt komendą `php skrypt.php Jan`, otrzymasz wynik:
```
Witaj, Jan!
```

## Zagłębiamy się:
Argumenty linii poleceń to stara sztuczka. UNIX i jego terminale używały tego dawno przed pojawieniem się PHP. W PHP, `$argc` reprezentuje liczbę argumentów, a `$argv` jest tablicą zawierającą same argumenty. Pierwszy element, `$argv[0]`, to zawsze nazwa bieżącego skryptu.

Alternatywami dla argumentów linii poleceń mogą być interaktywne dane wejściowe (np. `readline`), pliki konfiguracyjne lub zmienne środowiskowe. Kiedy jednak chcesz szybko przekazać wartości bez dodatkowych kombinacji, CLI (Command Line Interface) jest twoim najlepszym przyjacielem.

Na poziomie implementacji, warto pamiętać o `getopt()`, funkcji PHP, która pozwala na zaawansowane parsowanie argumentów linii poleceń, w tym opcji skróconych i długich. Jest przydatna, gdy masz do czynienia z bardziej skomplikowanymi skryptami CLI.
