---
title:                "PHP: Tworzenie pliku tymczasowego"
simple_title:         "Tworzenie pliku tymczasowego"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Oto kilka powodów, dla których warto poznać, jak tworzyć pliki tymczasowe w PHP:

 * Twórz bezpieczne i efektywne pliki tymczasowe, które są usuwane po użyciu
 * Zapobiegaj konfliktom nazw plików i błędów związanych z wieloma użytkownikami
 * Śledź działanie plików i usuń je, gdy nie są już potrzebne

## Jak to zrobić

Stwórz plik tymczasowy za pomocą wbudowanej funkcji `tmpfile()` i zapisz go w zmiennej. Następnie użyj funkcji `fwrite()` i `fread()` do zapisywania i odczytywania danych z tymczasowego pliku. Poniżej znajduje się przykład kodu, aby lepiej zrozumieć ten proces:

```PHP
$file = tmpfile();
fwrite($file, "To jest przykładowy tekst.");
rewind($file);
echo fread($file, filesize("file.txt"));
```

Kod ten utworzy nowy plik tymczasowy, zapisze w nim tekst i następnie odczyta go i wyświetli na stronie.

## Głębsza analiza

PHP zapewnia kilka wbudowanych funkcji do zarządzania plikami tymczasowymi. Na przykład, funkcja `tmpfile()` tworzy plik tymczasowy w katalogu systemowym null, a funkcja `fclose()` usuwa plik po użyciu. Możesz również ustawić unikalną nazwę dla tymczasowego pliku za pomocą funkcji `tempnam()`. 

Używając funkcji `fwrite()` i `fread()`, możesz zapisywać i odczytywać różne typy danych, takie jak tekst, liczby i obiekty. Ważne jest, aby pamiętać, że pliki tymczasowe są usuwane automatycznie po zakończeniu skryptu lub ręcznym zamknięciu przez funkcję `fclose()`.

## Zobacz także

Aby uzyskać więcej informacji na temat tworzenia plików tymczasowych w PHP, polecamy zapoznanie się z następującymi artykułami:

 * [Dokumentacja PHP na temat tworzenia plików tymczasowych](https://www.php.net/manual/en/function.tmpfile.php)
 * [Tworzenie plików tymczasowych w PHP - poradnik od trenera TechSith](https://www.youtube.com/watch?v=x3kkgpm8jAE)
 * [Przykładowy kod na GitHubie dla tworzenia plików tymczasowych w PHP](https://github.com/search?q=create+temporary+file+php)