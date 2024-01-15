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

# Dlaczego Sprawdzamy Istnienie Katalogu w PHP?

Sprawdzanie, czy katalog istnieje, jest nieodłącznym elementem programowania w PHP. Jest to niezbędne do wykonywania wielu codziennych zadań, takich jak tworzenie plików i katalogów, praca z systemem plików oraz przechowywanie i dostęp do danych. Dlatego warto poznać ten proces i umieć go wykonać w swoich projektach.

# Jak to zrobić?

```PHP
if(file_exists($path)){
    echo "Katalog istnieje";
} else{
    echo "Katalog nie istnieje";
}
```

Powyższy przykład wykorzystuje funkcję wbudowaną w PHP - `file_exists()`, która zwraca wartość logiczną true lub false w zależności od istnienia podanego w parametrze ścieżki. Jeśli katalog istnieje, zostanie wyświetlona wiadomość "Katalog istnieje", w przeciwnym razie zostanie wyświetlona wiadomość "Katalog nie istnieje".

Możemy również wykorzystać operator logiczny `!` do skrócenia kodu:

```PHP
if(!file_exists($path)){
    echo "Katalog nie istnieje";
} else{
    echo "Katalog istnieje";
}
```

Pamiętajmy również o sprawdzaniu, czy podana ścieżka jest właściwym katalogiem, a nie plikiem. Możemy to zrobić za pomocą funkcji `is_dir()`:

```PHP
if(is_dir($path)){
    echo "Jest to katalog";
} else{
    echo "Nie jest to katalog";
}
```

# Rzućmy okiem głębiej

Sprawdzenie istnienia katalogu może być też wykorzystane do innych celów, na przykład do ochrony plików przed dostępem wysłanym przez użytkownika. W takim przypadku możemy wykorzystać funkcję `is_writable()`, która zwróci true, jeśli katalog jest zapisywalny. To ważna funkcja, szczególnie w przypadku stron internetowych, gdzie bezpieczna obsługa plików i katalogów jest niezwykle istotna.

Należy również pamiętać, że funkcja `file_exists()` nie może sprawdzić istnienia katalogów z wykorzystaniem protokołu `http`. W przypadku, gdy potrzebujemy sprawdzić istnienie katalogu plików znajdujących się na zewnętrznym serwerze, możemy wykorzystać funkcję `get_headers()` i sprawdzić status odpowiedzi HTTP.

# Zobacz również
- Funkcja `file_exists()` w dokumentacji PHP: http://php.net/file_exists
- Przykłady wykorzystania funkcji `file_exists()` i `is_writable()`: https://www.php.net/filesystem
- Przydatne porady dotyczące bezpieczeństwa w PHP: https://www.acunetix.com/blog/articles/php-security-cheat-sheet/
- Dokumentacja funkcji `is_dir()`: http://php.net/is_dir