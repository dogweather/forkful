---
title:                "Odczyt pliku tekstowego"
html_title:           "PHP: Odczyt pliku tekstowego"
simple_title:         "Odczyt pliku tekstowego"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?
Czytanie pliku tekstowego to proces odczytywania zawartości pliku zapisanego w formacie tekstowym. Programiści często wykonują tę czynność w celu pobrania danych lub przetworzenia informacji z pliku tekstowego.

## Jak to zrobić?
```PHP
$file = fopen("plik.txt", "r"); //otwórz plik w trybie tylko do odczytu
if($file) { //sprawdź czy plik został poprawnie otwarty
    while(($line = fgets($file)) !== false) { //odczytaj plik linia po linii
        echo $line; //wyświetl linię
    }
    fclose($file); //zamknij plik
}
```
Przykładowa zawartość pliku.txt:
```
To jest tekst zapisany w pliku.
Odczytam go w moim programie PHP.
Ta linia zostanie wyświetlona jako pierwsza.
A ta jako druga.
```
Wynik:
```
To jest tekst zapisany w pliku.
Odczytam go w moim programie PHP.
Ta linia zostanie wyświetlona jako pierwsza.
A ta jako druga.
```

## Głębsze zanurzenie
Obecnie najczęściej stosowanym formatem danych jest format tekstowy. Dzięki niemu pliki są czytelne dla człowieka i prostsze w przetwarzaniu przez programy. Alternatywami dla czytania pliku tekstowego są na przykład czytanie pliku binarnego lub bazy danych. Implementacja czytania pliku tekstowego w PHP jest prosta i wykorzystuje funkcję ```fgets()```, która odczytuje plik linia po linii. W przeszłości, gdy format binarny był popularniejszy, konieczne było wykorzystanie innych funkcji, takich jak ```fread()```.

## Zobacz również
- Dokumentacja PHP o funkcji [```fgets()```](https://www.php.net/manual/en/function.fgets.php)
- Przykładowe użycie funkcji [```fgets()```](https://www.w3schools.com/php/func_filesystem_fgets.asp) w prostym programie PHP.