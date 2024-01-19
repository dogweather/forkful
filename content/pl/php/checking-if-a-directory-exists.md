---
title:                "Sprawdzanie, czy katalog istnieje"
html_title:           "Bash: Sprawdzanie, czy katalog istnieje"
simple_title:         "Sprawdzanie, czy katalog istnieje"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Sprawdzanie, czy katalog istnieje, to jest kluczowe zadanie w PHP. Programiści to robią, aby uniknąć błędów podczas manipulacji plikami i katalogami oraz zapewniania bezpiecznego dostępu do zasobów.

## Jak to zrobić:

Sprawdź, czy katalog istnieje za pomocą funkcji `is_dir()` w PHP. Na przykład:

```PHP
<?php
$dir_check = '/path/to/your/directory';

if (is_dir($dir_check)) {
    echo 'Katalog istnieje';
} else {
    echo 'Katalog nie istnieje';
}
?>
```

Jeśli katalog istnieje, na ekranie wyświetli się napis "Katalog istnieje". W przeciwnym razie pojawia się napis "Katalog nie istnieje".

## Głębsze zrozumienie

Sprawdzanie, czy katalog istnieje, rozpoczęło się w PHP 4, kiedy wprowadzono funkcję `is_dir()`. Istnieją alternatywy jak `file_exists()`, ale ta funkcja sprawdza zarówno pliki, jak i katalogi, co może prowadzić do niejasności. `is_dir()` jest preferowane, ponieważ jest jasne, co sprawdza. Oczywiście kod wykonuje się szybciej, jeśli nie musimy sprawdzać, czy coś jest plikiem, czy katalogiem.

## Zobacz także

Istnieje wiele źródeł do nauki PHP i manipulacji plikami i katalogami. Tutaj niektóre z nich:

- Oficjalna dokumentacja PHP na `is_dir()`: [Link do dokumentacji](https://www.php.net/manual/en/function.is-dir.php)
- Oficjalna dokumentacja PHP na `file_exists()`: [Link do dokumentacji](https://www.php.net/manual/en/function.file-exists.php)
- Wspaniały tutorial na stronie PHP.net o manipulacji plikami i katalogami: [Link do tutoriala](https://www.php.net/manual/en/intro.filesystem.php)