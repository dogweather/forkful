---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:50.985310-07:00
description: "Jak to zrobi\u0107: PHP natywnie obs\u0142uguje zapisywanie do pliku\
  \ za pomoc\u0105 funkcji takich jak `file_put_contents`, `fopen` w po\u0142\u0105\
  czeniu z `fwrite` oraz\u2026"
lastmod: '2024-03-13T22:44:35.515909-06:00'
model: gpt-4-0125-preview
summary: "PHP natywnie obs\u0142uguje zapisywanie do pliku za pomoc\u0105 funkcji\
  \ takich jak `file_put_contents`, `fopen` w po\u0142\u0105czeniu z `fwrite` oraz\
  \ `fclose`."
title: Pisanie pliku tekstowego
weight: 24
---

## Jak to zrobić:
PHP natywnie obsługuje zapisywanie do pliku za pomocą funkcji takich jak `file_put_contents`, `fopen` w połączeniu z `fwrite` oraz `fclose`. Oto jak z nich korzystać:

### Proste zapisywanie z użyciem `file_put_contents`:
Ta funkcja upraszcza proces zapisu do pliku, robiąc wszystko w jednym kroku.
```php
$content = "Witaj, świecie!";
file_put_contents("hello.txt", $content);
// Sprawdza, czy plik został pomyślnie zapisany
if (file_exists("hello.txt")) {
    echo "Plik został pomyślnie utworzony!";
} else {
    echo "Nie udało się utworzyć pliku.";
}
```

### Zaawansowane zapisywanie z użyciem `fopen`, `fwrite` oraz `fclose`:
Dla większej kontroli nad zapisywaniem do pliku, takiego jak dodawanie tekstu czy bardziej zaawansowane obsługiwanie błędów, użyj `fopen` z `fwrite`.
```php
$file = fopen("hello.txt", "a"); // tryb 'a' do dołączania, 'w' do zapisu
if ($file) {
    fwrite($file, "\nDodawanie więcej treści.");
    fclose($file);
    echo "Treść dodana pomyślnie!";
} else {
    echo "Nie udało się otworzyć pliku.";
}
```

#### Odczytanie pliku do wyświetlenia:
Aby zweryfikować naszą treść:
```php
echo file_get_contents("hello.txt");
```
**Przykładowe wyjście:**
```
Witaj, świecie!
Dodawanie więcej treści.
```

### Korzystanie z bibliotek firm trzecich:
Dla bardziej skomplikowanych operacji na plikach można użyć bibliotek takich jak `League\Flysystem` dla warstwy abstrakcji nad systemem plików, ale wbudowane funkcje PHP często wystarczają dla podstawowych zadań związanych z zapisywaniem do pliku. Oto krótki przykład, jeśli zdecydujesz się zbadać `Flysystem`:
```php
require 'vendor/autoload.php';
use League\Flysystem\Filesystem;
use League\Flysystem\Local\LocalFilesystemAdapter;

$adapter = new LocalFilesystemAdapter(__DIR__);
$filesystem = new Filesystem($adapter);

$filesystem->write('hello.txt', "Korzystając z Flysystem do zapisu tego.");
```
Przykład zakłada, że zainstalowano `league/flysystem` za pomocą Composera. Biblioteki firm trzecich mogą znacznie uprościć bardziej skomplikowane operacje na plikach, zwłaszcza przy pracy z różnymi systemami przechowywania w sposób płynny.
