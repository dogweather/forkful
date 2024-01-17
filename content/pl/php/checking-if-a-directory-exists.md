---
title:                "Sprawdzanie czy istnieje katalog"
html_title:           "PHP: Sprawdzanie czy istnieje katalog"
simple_title:         "Sprawdzanie czy istnieje katalog"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Sprawdzanie, czy katalog istnieje, to w prostych słowach jest to działanie, które pozwala programistom upewnić się, że katalog istnieje, zanim spróbują z nim coś zrobić. Jest to ważna część procesu pisania kodu, ponieważ pozwala uniknąć błędów i nieprzewidzianych problemów.

## Jak to zrobić:
Sprawdzenie, czy katalog istnieje w PHP jest bardzo proste. Aby to zrobić, wystarczy użyć funkcji `is_dir()`, która przyjmuje jako argument ścieżkę do katalogu. Jeśli katalog istnieje, funkcja zwróci wartość `true`, a w przeciwnym razie `false`. Przykładowe użycie tej funkcji prezentuje się następująco:

```PHP
if(is_dir("/sciezka/do/katalogu")){
    echo "Katalog istnieje";
}else{
    echo "Katalog nie istnieje";
}
```

Przykładowe wyjście: `Katalog istnieje`

## Zanurzenie:
Sprawdzanie istnienia katalogu jest ważne od bardzo dawna, ponieważ pozwala uniknąć wielu problemów związanych z bezpieczeństwem i wydajnością. Istnieją jednak także inne sposoby na to, aby upewnić się, że katalog istnieje, na przykład za pomocą funkcji `file_exists()`. Różnicę między tymi dwiema funkcjami można poznać poprzez wywołanie funkcji `is_dir()` na nieistniejącym pliku, co zwróci wartość `false`, podczas gdy wywołanie `file_exists()` zwróci informację o błędzie.

Jeśli chodzi o implementację, funkcja `is_dir()` korzysta z systemowego wywołania `stat`, które zwraca informacje o danym pliku lub katalogu. Jest to szybki sposób na sprawdzenie istnienia katalogu, ponieważ nie ma potrzeby odczytywania zawartości katalogu.

## Zobacz także:
- Dokumentacja PHP o funkcji `is_dir()`: https://www.php.net/manual/en/function.is-dir.php
- Porównanie `is_dir()` i `file_exists()`: https://stackoverflow.com/questions/3284563/php-file-exists-vs-is-dir
- Funkcja `stat` w dokumentacji PHP: https://www.php.net/manual/en/function.stat.php