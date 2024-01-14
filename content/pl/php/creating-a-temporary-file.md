---
title:                "PHP: Tworzenie pliku tymczasowego"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Tworzenie tymczasowych plików jest niezwykle przydatną umiejętnością w pracy z PHP. Może być wiele powodów, dla których będziesz chciał stworzyć plik tymczasowy, ale najczęściej jest to potrzebne do przechowywania lub przetwarzania danych w sposób tymczasowy.

## Jak To Zrobić

Tworzenie tymczasowych plików w PHP jest bardzo proste za pomocą funkcji `tempnam()` oraz `tmpfile()`. Poniżej przedstawione są dwa przykłady kodu, które pokazują, jak używać tych funkcji i jak wygląda ich wynik.

### Przykład 1: Użycie funkcji `tempnam()`

```PHP
$tempFile = tempnam(sys_get_temp_dir(), "prefix_");
echo $tempFile;
```

Wyjście:
`/var/folders/lr/0yzg90s90k1d9bdz7stp0qzjsqzo3/temp_prefix_`

### Przykład 2: Użycie funkcji `tmpfile()`

```PHP
$tempFile = tmpfile();
echo fwrite($tempFile, "Dane dla pliku tymczasowego");
echo stream_get_meta_data($tempFile)['uri'];
```

Wyjście:
`/var/folders/lr/0yzg90s90k1d9bdz7stp0qzjsqzo3/znyErH`

W pierwszym przykładzie, użyliśmy funkcji `tempnam()`, aby utworzyć plik tymczasowy o określonym prefiksie w katalogu, który może być odczytany przez system. W drugim przykładzie, użyliśmy funkcji `tmpfile()`, która tworzy tymczasowy plik w pamięci RAM, z którym możemy pracować za pomocą standardowych funkcji PHP takich jak `fwrite()` czy `fread()`.

## Deep Dive

Podczas gdy tworzenie tymczasowych plików jest proste, warto wiedzieć nieco więcej o tym temacie. Istnieją różne sposoby tworzenia tymczasowych plików w PHP, takie jak wykorzystanie funkcji `uniqid()` lub `microtime()` w połączeniu z prefiksem. Możesz również ustalić lokalizację tymczasowych plików przez zmianę ustawień środowiskowych PHP, korzystając z funkcji `putenv()`.

Należy jednak pamiętać, że tymczasowe pliki powinny być używane tylko w celach tymczasowych. Należy się upewnić, że są one usuwane po zakończeniu pracy z nimi, aby uniknąć zapełnienia dysku twardego.

## Zobacz także

- [Dokumentacja PHP - Funkcja `tempnam()`](https://www.php.net/manual/pl/function.tempnam.php)
- [Dokumentacja PHP - Funkcja `tmpfile()`](https://www.php.net/manual/pl/function.tmpfile.php)
- [Artykuł na temat tworzenia tymczasowych plików w PHP](https://www.taniarascia.com/create-temporary-files-in-php/)