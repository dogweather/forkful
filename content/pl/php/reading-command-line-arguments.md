---
title:                "Odczytywanie argumentów linii poleceń"
date:                  2024-01-20T17:56:28.647491-07:00
model:                 gpt-4-1106-preview
simple_title:         "Odczytywanie argumentów linii poleceń"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Czytanie argumentów linii poleceń to pobieranie danych wejściowych od użytkownika podczas wykonywania skryptu PHP w terminalu. Programiści wykorzystują tę technikę, gdy chcą, aby ich skrypt był elastyczny i mógł być łatwo uruchamiany z różnymi parametrami.

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
