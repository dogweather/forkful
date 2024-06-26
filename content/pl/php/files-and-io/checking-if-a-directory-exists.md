---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:26.047038-07:00
description: "Jak to zrobi\u0107: Natywnym sposobem na sprawdzenie, czy katalog istnieje\
  \ w PHP, jest u\u017Cycie funkcji `is_dir()`. Funkcja ta przyjmuje \u015Bcie\u017C\
  k\u0119 do katalogu jako\u2026"
lastmod: '2024-03-13T22:44:35.511869-06:00'
model: gpt-4-0125-preview
summary: "Natywnym sposobem na sprawdzenie, czy katalog istnieje w PHP, jest u\u017C\
  ycie funkcji `is_dir()`."
title: Sprawdzanie, czy katalog istnieje
weight: 20
---

## Jak to zrobić:
Natywnym sposobem na sprawdzenie, czy katalog istnieje w PHP, jest użycie funkcji `is_dir()`. Funkcja ta przyjmuje ścieżkę do katalogu jako argument i zwraca `true`, jeśli katalog istnieje i jest katalogiem, lub `false` w przeciwnym razie.

```php
$directoryPath = "/ścieżka/do/twojego/katalogu";

if(is_dir($directoryPath)) {
    echo "Katalog istnieje.";
} else {
    echo "Katalog nie istnieje.";
}
```

Przykładowe Wyjście:
```
Katalog istnieje.
```
Lub, jeśli katalog nie istnieje:
```
Katalog nie istnieje.
```

Chociaż standardowa biblioteka PHP jest wystarczająco rozbudowana dla większości zadań związanych z manipulacją katalogami i plikami, czasami możesz potrzebować bardziej kompleksowego rozwiązania. W takich przypadkach popularną biblioteką zewnętrzną jest komponent Filesystem od Symfony. Oferuje on szeroki zakres narzędzi do pracy z systemem plików, w tym prosty sposób na sprawdzenie, czy katalog istnieje.

Najpierw musisz zainstalować komponent Filesystem od Symfony. Jeśli używasz Composer (menedżera zależności dla PHP), możesz uruchomić następujące polecenie w katalogu swojego projektu:

```
composer require symfony/filesystem
```

Po zainstalowaniu komponentu Filesystem od Symfony, możesz użyć go do sprawdzenia, czy katalog istnieje, w następujący sposób:

```php
use Symfony\Component\Filesystem\Filesystem;

$filesystem = new Filesystem();
$directoryPath = '/ścieżka/do/twojego/katalogu';

if($filesystem->exists($directoryPath)) {
    echo "Katalog istnieje.";
} else {
    echo "Katalog nie istnieje.";
}
```

Przykładowe Wyjście:
```
Katalog istnieje.
```
Lub, jeśli katalog nie istnieje:
```
Katalog nie istnieje.
```

Obie metody zapewniają wiarygodny sposób na sprawdzenie istnienia katalogu w PHP. Wybór między używaniem wbudowanych funkcji PHP a biblioteką zewnętrzną jak komponent Filesystem od Symfony zależy od konkretnych potrzeb twojego projektu i tego, czy wymagasz dodatkowych manipulacji systemem plików, które mogą być efektywniej obsłużone przez bibliotekę.
