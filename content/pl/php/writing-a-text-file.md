---
title:                "Tworzenie pliku tekstowego"
html_title:           "PHP: Tworzenie pliku tekstowego"
simple_title:         "Tworzenie pliku tekstowego"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie plików tekstowych jest nieodłączną częścią programowania w PHP. Dzięki nim możemy zapisywać dane, tworzyć pliki konfiguracyjne lub też generować raporty. Jest to również podstawowa umiejętność potrzebna w wielu projektach, dlatego warto poznać tę technikę.

## Jak to zrobić

Pisanie plików tekstowych w PHP jest bardzo proste. Wystarczy wykorzystać funkcję `file_put_contents()` z podanym parametrem w postaci ścieżki do pliku oraz zawartości, którą chcemy zapisać.

```PHP
file_put_contents("plik.txt", "To jest przykładowy tekst");
```

Jeśli chcemy dopisywać dane do istniejącego pliku, musimy dodać jeszcze trzeci parametr `FILE_APPEND`.

```PHP
file_put_contents("plik.txt", "Kolejny tekst", FILE_APPEND);
```

Możemy również wykorzystać funkcję `fopen()` oraz `fwrite()`.

```PHP
$plik = fopen("plik.txt", "w"); // otwieramy plik w trybie zapisu
fwrite($plik, "To jest przykładowy tekst"); // zapisujemy dane
fclose($plik); // zamykamy plik
```

## Deep Dive

Podczas zapisu pliku warto zwrócić uwagę na kilka istotnych kwestii. Przede wszystkim należy upewnić się, że podana ścieżka jest prawidłowa oraz że plik, do którego chcemy zapisać dane, istnieje. Jeśli plik nie istnieje, zostanie automatycznie utworzony przez funkcję `file_put_contents()`.

W przypadku wykorzystania funkcji `fopen()` i `fwrite()` musimy pamiętać o zamknięciu pliku. Niezamknięty plik może wpłynąć na wydajność naszego kodu, dlatego warto używać funkcji `fclose()`.

Istotnym elementem pisania plików tekstowych jest również obsługa błędów. W przypadku błędów podczas zapisu, należy wyświetlić odpowiedni komunikat oraz ewentualnie podjąć działanie naprawcze.

## Zobacz również

- [Dokumentacja PHP: file_put_contents()](https://www.php.net/manual/pl/function.file-put-contents.php)
- [Dokumentacja PHP: fopen()](https://www.php.net/manual/pl/function.fopen.php)
- [Dokumentacja PHP: fwrite()](https://www.php.net/manual/pl/function.fwrite.php)