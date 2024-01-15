---
title:                "Tworzenie pliku tymczasowego"
html_title:           "PHP: Tworzenie pliku tymczasowego"
simple_title:         "Tworzenie pliku tymczasowego"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Tworzenie tymczasowych plików jest nieodłączną częścią tworzenia aplikacji webowych. Często wykorzystuje się je do przechowywania danych tymczasowych lub do przenoszenia plików na serwer. Są one również przydatne przy testowaniu kodu lub do przechowywania buforowanych danych.

## Jak to zrobić

Tworzenie tymczasowego pliku w PHP jest bardzo proste. Wystarczy użyć funkcji `tmpfile()` lub `tempnam()`, które automatycznie tworzą i zwracają uchwyt do tymczasowego pliku. Następnie możemy dodać dane do pliku za pomocą funkcji `fwrite()`.

```PHP
$file = tmpfile();
fwrite($file, "To jest przykładowy tekst.");
```

Możemy również wykorzystać funkcję `fputs()` do zapisywania danych do tymczasowego pliku za pomocą jednej linii kodu.

```PHP
$file = tempnam(sys_get_temp_dir(), 'example');
fputs($file, "To jest kolejny przykładowy tekst.");
```

Aby odczytać dane z tymczasowego pliku, możemy użyć funkcji `fread()` lub `fgets()`. Po zakończeniu pracy z plikiem, należy go usunąć za pomocą funkcji `fclose()`, która jednocześnie go zamyka.

```PHP
$handle = fopen($file, "r");
$data = fread($handle, filesize($file));
echo $data;

fclose($handle);
unlink($file);
```

## Głębsze zagadnienia

Tworzenie tymczasowych plików może być szczególnie przydatne, gdy pracujemy z dużymi plikami lub potrzebujemy wykonać operacje na plikach bezpośrednio na serwerze. W takich przypadkach tworzenie i przechowywanie danych w pamięci podręcznej jest nieefektywne i można to osiągnąć poprzez wykorzystanie tymczasowych plików.

Warto również pamiętać, że funkcja `tmpfile()` tworzy tymczasowy plik w pamięci, co oznacza, że jego zawartość zostanie utracona po wyłączeniu lub restartowaniu serwera. Jeśli chcemy zachować dane tymczasowego pliku, możemy wykorzystać funkcję `tempnam()`, aby utworzyć plik w określonej lokalizacji, takiej jak folder tymczasowy na serwerze.

## Zobacz również

- [Dokumentacja PHP na temat tworzenia tymczasowych plików](https://www.php.net/manual/en/function.tmpfile.php)
- [Artykuł o przechowywaniu danych za pomocą tymczasowych plików w PHP](https://www.cloudways.com/blog/php-temporary-file/)
- [Poradnik na temat operacji na plikach w PHP](https://www.w3schools.com/php/php_file.asp)