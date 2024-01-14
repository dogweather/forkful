---
title:    "PHP: Tworzenie tymczasowego pliku"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Dlaczego

Tworzenie pliku tymczasowego jest niezbędnym elementem wielu projektów PHP. Dzięki temu narzędziu możemy przechowywać dane tymczasowo, zanim zostaną one zapisane na stałe. Jest to bardzo przydatne w przypadku dużych ilości danych lub w trakcie przetwarzania danych w dużej ilości pamięci, gdy potrzebujemy zapisać je w bezpieczny sposób. W tym artykule pokażę wam, jak stworzyć plik tymczasowy za pomocą PHP oraz pokażę przykładowe kody i wyniki.

## Jak To Zrobić

Korzystając z PHP, możemy stworzyć plik tymczasowy za pomocą funkcji `tmpfile()`. Spójrz na poniższy kod:

```PHP
$file = tmpfile();
```

Funkcja `tmpfile()` automatycznie tworzy i otwiera plik w trybie "wb+". Możemy do niego zapisać dane używając funkcji `fputs()`:

```PHP
$file = tmpfile();
fputs($file, "To jest przykładowy tekst");
```

Jeśli chcemy odczytać dane z pliku tymczasowego, możemy użyć funkcji `fread()`:

```PHP
$file = tmpfile();
fputs($file, "To jest przykładowy tekst");
rewind($file); // potrzebne, aby przesunąć się na początek pliku
echo fread($file, filesize($file));
```

Kod powyżej wyświetli "To jest przykładowy tekst" w konsoli lub na stronie internetowej.

## Deep Dive

Funkcja `tmpfile()` tworzy plik w katalogu tymczasowym serwera, więc nie musimy się martwić o nazwy plików. Plik ten będzie automatycznie usuwany po zakończeniu skryptu lub zamknięciu uchwytu do pliku. Jednak jeśli chcemy sami określić nazwę i lokalizację pliku tymczasowego, możemy użyć innego sposobu - `tempnam()`.

```PHP
$file = tempnam("/tmp", "TMP_"); // plik zostanie utworzony w folderze /tmp o nazwie TMP_rand123
```

Funkcja `tempnam()` zwraca pełną ścieżkę do utworzonego pliku, więc możemy bezpośrednio do niego odwoływać się używając standardowych funkcji PHP zapisu i odczytu plików.

## Zobacz również

- [Oficjalna dokumentacja PHP - funkcja tmpfile()](https://www.php.net/manual/en/function.tmpfile.php)
- [Oficjalna dokumentacja PHP - funkcja tempnam()](https://www.php.net/manual/en/function.tempnam.php)
- [Jak stworzyć plik tymczasowy w PHP](https://www.w3schools.com/php/func_filesystem_tempnam.asp)