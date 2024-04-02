---
date: 2024-01-20 17:41:21.884177-07:00
description: "Tworzymy pliki tymczasowe, gdy musimy przechowa\u0107 dane tymczasowo,\
  \ zanim uko\u0144czymy z nimi co\u015B konkretnego. Programi\u015Bci robi\u0105\
  \ to, by zarz\u0105dza\u0107 danymi\u2026"
lastmod: '2024-03-13T22:44:35.516862-06:00'
model: gpt-4-1106-preview
summary: "Tworzymy pliki tymczasowe, gdy musimy przechowa\u0107 dane tymczasowo, zanim\
  \ uko\u0144czymy z nimi co\u015B konkretnego. Programi\u015Bci robi\u0105 to, by\
  \ zarz\u0105dza\u0107 danymi\u2026"
title: Tworzenie pliku tymczasowego
weight: 21
---

## Co i dlaczego?
Tworzymy pliki tymczasowe, gdy musimy przechować dane tymczasowo, zanim ukończymy z nimi coś konkretnego. Programiści robią to, by zarządzać danymi tymczasowymi w sposób bezpieczny, izolując je od reszty systemu, co jest kluczowe przy operacjach, które są wrażliwe na zakłócenia, takie jak transakcje czy przetwarzanie danych w tle.

## Jak to zrobić:
W PHP robienie plików tymczasowych jest proste. Użyj `tmpfile()` do stworzenia i otwarcia pliku, a on sam się usunie po zamknięciu, albo `tempnam()` do stworzenia pliku tymczasowego z unikalną nazwą.

```php
<?php
// Użycie tmpfile()
$temp = tmpfile();
fwrite($temp, "Tu jest coś tymczasowego.\n");
rewind($temp); // Powrót na początek pliku
echo fread($temp, 1024); // Wypisanie zawartości pliku
fclose($temp); // Plik jest usunięty

// Użycie tempnam()
$tmpDir = sys_get_temp_dir(); // Pobieranie ścieżki do folderu tymczasowego
$tempName = tempnam($tmpDir, 'moj_prefiks_');
$file = fopen($tempName, "w");
fwrite($file, "Przechowuje to na później.\n");
fclose($file); // Pamietaj by ręcznie usunąć plik, gdy skończysz.
unlink($tempName); // Usuwanie pliku
?>
```

## Głębsze spojrzenie:
Tworzenie plików tymczasowych w PHP ma swoje korzenie w dawnych sposobach zarządzania danymi. Historia uczy nas, że unikanie konfliktu nazw i zapisywania danych do bufora na dysku to fundamenty pracy z plikami tymczasowymi. Alternatywami dla `tmpfile()` i `tempnam()` są własne skrypty zarządzające plikami tymczasowymi, ale dbaj o unikalność nazw, bezpieczne wyczyszczenie i izolację danych. Co do implementacji, `tmpfile()` automatycznie usuwa plik po zamknięciu strumienia, a `tempnam()` wymaga od ciebie manulnego usunięcia pliku po zakończonej pracy. Wybór metody zależy od przypadku użycia i potrzeby kontroli nad plikiem.

## Zobacz także:
- Dokumentacja PHP dla `tmpfile()`: https://www.php.net/manual/en/function.tmpfile.php
- Dokumentacja PHP dla `tempnam()`: https://www.php.net/manual/en/function.tempnam.php
- Dyskusja na temat zarządzania plikami tymczasowymi w PHP: https://stackoverflow.com/questions/1707801/making-a-temporary-dir-for-unpacking-a-zipfile-into
- `sys_get_temp_dir()`, by znaleźć domyślny folder tymczasowy: https://www.php.net/manual/en/function.sys-get-temp-dir.php
