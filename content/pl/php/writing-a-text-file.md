---
title:                "Zapisywanie pliku tekstowego"
date:                  2024-01-19
html_title:           "Arduino: Zapisywanie pliku tekstowego"
simple_title:         "Zapisywanie pliku tekstowego"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Zapisywanie pliku tekstowego to proces, w którym tworzy się lub modyfikuje plik przechowujący dane tekstowe. Programiści robią to, by trwale zapisywać dane, takie jak logi, konfiguracje, czy wyniki pracy aplikacji.

## Jak to zrobić:
```PHP
<?php
$tekst = "Witaj, świat programowania PHP!";
$plik = "przykladowy_plik.txt";

// Otwieranie pliku do zapisu
if ($uchwyt = fopen($plik, 'w')) { 
    // Zapisywanie tekstu i sprawdzanie powodzenia
    if (fwrite($uchwyt, $tekst)) {
        echo "Zapisano: $tekst\n";
    } else {
        echo "Nie udało się zapisać danych.\n";
    }
    fclose($uchwyt);
} else {
    echo "Nie można otworzyć pliku.\n";
}
?>
```
Sample output:
```
Zapisano: Witaj, świat programowania PHP!
```

## Deep Dive
PHP umożliwia zapis plików od swoich pierwszych wersji, bo jest to podstawowa funkcjonalność w pracy z danymi. Alternatywą jest użycie funkcji `file_put_contents()`, która jest prostsza w obsłudze, ale oferuje mniej kontrolę. Zapisywając pliki, należy pamiętać o odpowiednich uprawnieniach dostępu do pliku oraz możliwych problemach z bezpieczeństwem przy zapisie danych pochodzących od użytkowników.

## See Also
- Dokumentacja PHP na temat obsługi plików: [php.net/manual/en/book.filesystem.php](https://www.php.net/manual/en/book.filesystem.php)
- Wprowadzenie do bezpieczeństwa plików w PHP: [php.net/manual/en/security.filesystem.php](https://www.php.net/manual/en/security.filesystem.php)
- Artykuł o różnych sposobach zapisu danych w PHP: [phptherightway.com/#files](http://phptherightway.com/#files)
