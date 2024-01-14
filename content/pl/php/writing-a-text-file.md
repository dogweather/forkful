---
title:                "PHP: Pisanie pliku tekstowego"
simple_title:         "Pisanie pliku tekstowego"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie plików tekstowych jest nieodłączną częścią tworzenia programów w PHP. Pozwala nam na przechowywanie i przetwarzanie danych w czytelnej i łatwo dostępnej formie. Dzięki temu możemy mieć kontrolę nad informacjami, które zostaną wykorzystane w naszych aplikacjach.

## Jak to zrobić

Aby zapisać dane do pliku tekstowego w PHP, musimy wykorzystać funkcję `fwrite()`. Przykładowy kod wygląda następująco:

```PHP
$file = fopen("plik.txt", "w"); // otwarcie pliku w trybie zapisu
fwrite($file, "To jest przykładowy tekst, który zostanie zapisany do pliku."); // zapisanie tekstu
fclose($file); // zamknięcie pliku
```

Jeśli chcemy dopisywać nowe informacje do istniejącego pliku, wystarczy zmienić tryb otwarcia na `a`. Dodatkowo, dla pełnej kontroli nad sposobem zapisywania danych, możemy wykorzystać funkcję `file_put_contents()`.

```PHP
$text = "Kolejny przykładowy tekst.";

// zapisywanie do pliku
file_put_contents("plik.txt", $text, FILE_APPEND);
```

## Głębsze zanurzenie

Podczas pisania plików tekstowych, warto zwrócić uwagę na kilka ważnych kwestii. Po pierwsze, należy pamiętać o poprawnej deklaracji kodowania znaków. Warto również zadbać o odpowiednie formatowanie tekstu, aby był czytelny i przejrzysty. W przypadku dużych plików warto rozważyć podział danych na mniejsze części lub wykorzystanie dodatkowych narzędzi, takich jak bazy danych.

## Zobacz również

- Dokumentacja PHP dla funkcji [fwrite()](https://www.php.net/manual/en/function.fwrite.php)
- Przykładowe zadania związane z pisanie plików tekstowych w [Codecademy](https://www.codecademy.com/courses/learn-php/lessons/introduction-to-php-commands/exercises/writing-to-file)
- Poradnik na [GitHub](https://gist.github.com/riccardoforina/713c0040a28a6f4e0643423dfc797e62) dotyczący pisanie tekstu do pliku w PHP