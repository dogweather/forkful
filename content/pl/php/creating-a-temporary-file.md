---
title:                "Tworzenie tymczasowego pliku"
html_title:           "C#: Tworzenie tymczasowego pliku"
simple_title:         "Tworzenie tymczasowego pliku"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Tworzenie tymczasowego pliku to proces, który produkuje plik o unikalnej nazwie, stale utworzony, ale przeznaczony do usunięcia po zakończeniu sesji. Programiści robią to usuwając ryzyko konfliktu nazw plików i zapewniając bezpieczeństwo danych, ograniczając dostęp do plików tymczasowych.

## Jak to zrobić:

Tworzenie plików tymczasowych w PHP jest proste. Zaczynamy od funkcji `tempnam()`, która tworzy unikalną nazwę pliku.

```PHP
<?php
$tmpfname = tempnam("/tmp", "FOO");
$handle = fopen($tmpfname, "w");
fwrite($handle, "test writing to a temporary file");
fclose($handle);
?>
```
Aby przeczytać plik, użyjemy `fopen()` i `fread()`:

```PHP
<?php
$handle = fopen($tmpfname, "r");
$contents = fread($handle, filesize($tmpfname));
fclose($handle);
echo $contents; // Outputs: test writing to a temporary file
```

## Pod wodą

Tworzenie tymczasowych plików to stary trik w programowaniu, mocno osadzony w PHP od jego pierwszych wersji. Do alternatywnych metod należy korzystanie z pamięci masowej `tmpfile()`, która jest mniej skomplikowana, ale oferuje mniej kontroli. Od strony implementacji, PHP generuje nazwę pliku tymczasowego poprzez sklejenie określonego prefiksu z unikalnym identyfikatorem.

## Zobacz również

* [Dokumentacja PHP dla tempnam()](https://www.php.net/manual/en/function.tempnam.php)
* [Dokumentacja PHP dla tmpfile()](https://www.php.net/manual/en/function.tmpfile.php)
* [Jak bezpiecznie korzystać z plików tymczasowych w PHP – na StackOverflow](https://stackoverflow.com/questions/377279/are-php-temporary-file-name-functions-secure)