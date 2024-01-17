---
title:                "Pisanie pliku tekstowego"
html_title:           "PHP: Pisanie pliku tekstowego"
simple_title:         "Pisanie pliku tekstowego"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Tworzenie pliku tekstowego to proces, w którym umieszczamy dane w formie tekstu w pliku na naszym komputerze. Programiści stosują to jako część swojego kodu, ponieważ pozwala to na przechowywanie danych poza programem i ułatwia późniejsze ich wykorzystanie.

## Jak to zrobić?

Możemy użyć funkcji *file_put_contents()* w PHP, aby utworzyć plik tekstowy i wprowadzić do niego dane. Sprawdźmy poniższy przykład:

```PHP
<?php
$filename = "tekstowy_plik.txt"; // ustalamy nazwę pliku
$content = "Przykładowy tekst do zapisania w pliku"; // ustawiamy zawartość, którą chcemy zapisać

if(file_put_contents($filename, $content)) { // sprawdzamy, czy plik został utworzony
  echo "Plik został utworzony i zapisany z powodzeniem.";
} else {
  echo "Wystąpił błąd podczas próby utworzenia pliku.";
}
?>
```

Po wykonaniu tego kodu, powinniśmy mieć nowy plik tekstowy o nazwie *tekstowy_plik.txt* z zawartością "Przykładowy tekst do zapisania w pliku".

## Głębszy zanurzenie

Tworzenie pliku tekstowego nie jest niczym nowym w świecie programowania. Przez wiele lat programiści używali tej techniki do przechowywania danych bezpośrednio na komputerze bez konieczności korzystania z bazy danych. Inną popularną metodą jest wykorzystanie funkcji *fopen()* do otwierania pliku i dopisywania danych do niego z użyciem funkcji *fwrite()*. Zarówno *file_put_contents()* jak i *fopen()* mogą być używane do tworzenia plików tekstowych, jednak pierwsza jest prostszą i bardziej czytelną opcją.

## Zobacz też

Więcej informacji oraz dokładniejsze wyjaśnienie funkcji *file_put_contents()* znajdziesz na oficjalnej dokumentacji PHP: https://www.php.net/manual/en/function.file-put-contents.php