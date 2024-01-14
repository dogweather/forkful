---
title:    "PHP: Tworzenie pliku tymczasowego"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego tworzenie tymczasowych plików jest ważne?

Tworzenie plików tymczasowych jest niezbędnym krokiem w wielu projektach programistycznych. Często potrzebujemy pamiętnego miejsca do przechowywania ważnych danych lub tymczasowych wyników obliczeń. Tworzenie tymczasowych plików daje nam kontrolę nad tym, gdzie i jak te dane są przechowywane, co jest szczególnie ważne w sprawach bezpieczeństwa.

## Jak to zrobić?

Oto krótki przykład kodu, który pokazuje, jak w prosty sposób utworzyć tymczasowy plik w języku PHP:

```PHP
$fileName = tempnam(sys_get_temp_dir(), "temporary_file");
$file = fopen($fileName, "w");
fwrite($file, "Jestem tymczasowym plikiem!");
fclose($file);
echo $fileName; // Output: ścieżka/do/tymczasowego_pliku5457658
```

W powyższym przykładzie użyliśmy funkcji `tempnam()` do utworzenia nazwy dla naszego tymczasowego pliku. Dodatkowo, korzystając z funkcji `sys_get_temp_dir()`, ustaliliśmy, że nasz plik zostanie zapisany w domyślnym katalogu dla plików tymczasowych. Następnie, za pomocą funkcji `fopen()`, otwieramy nasz plik w trybie do zapisu (opcja "w"). Warto zauważyć, że jest to tylko jeden z wielu sposobów na utworzenie tymczasowego pliku w PHP.

Kiedy już mamy otwarty plik, możemy z niego korzystać tak, jak z każdego innego pliku. W tym przykładzie, za pomocą funkcji `fwrite()`, zapisujemy do niego krótki napis. Na koniec, zamykamy plik, używając funkcji `fclose()`.

## Głębsza analiza

Tworząc tymczasowy plik, musimy pamiętać o kilku ważnych kwestiach. Po pierwsze, należy zawsze upewnić się, że utworzyliśmy unikalną nazwę dla naszego pliku, aby uniknąć kolizji nazw w naszym systemie plików. W tym celu możemy użyć funkcji `uniqid()` lub `md5()`.

Po drugie, zawsze należy zadbać o bezpieczeństwo tworzonego pliku. Nie należy pozostawiać go na serwerze zdalnym, ponieważ może to prowadzić do problemów z bezpieczeństwem. Zaleca się usuwanie tymczasowych plików po ich wykorzystaniu, aby uniknąć zagrożenia przez złośliwe oprogramowanie.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej o tworzeniu tymczasowych plików w PHP, polecamy zapoznanie się z oficjalną dokumentacją języka oraz poniższymi artykułami:

- [Funkcja tempfile() w PHP](https://www.php.net/manual/pl/function.tempfile.php)
- [Tymczasowe i stałe pliki w PHP](https://netbeans.org/kb/docs/php/temporary-files.html)
- [Bezpieczeństwo plików tymczasowych w PHP](https://www.php.net/manual/pl/function.tempnam.php#example-1115)