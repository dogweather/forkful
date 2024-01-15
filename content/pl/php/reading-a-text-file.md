---
title:                "Odczytywanie pliku tekstowego"
html_title:           "PHP: Odczytywanie pliku tekstowego"
simple_title:         "Odczytywanie pliku tekstowego"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek miałeś problem z odczytaniem danych z pliku tekstowego w języku PHP? Jeśli tak, to ten artykuł jest dla ciebie! Przeczytasz tutaj jak w prosty sposób można odczytać dane z pliku tekstowego za pomocą aktualnej wersji PHP. 

## Jak To zrobić?

### Przygotowanie pliku tekstowego

Najpierw utwórzmy prosty plik tekstowy do odczytu. Może to być plik z rozszerzeniem `.txt` lub `.csv` z dowolnymi danymi. Ważne jest, aby pamiętać, że plik musi być dostępny w katalogu, w którym znajduje się nasz kod PHP.

### Kod

Teraz przejdźmy do sedna artykułu - kodu PHP. W poniższym przykładzie użyjemy funkcji `fopen()` do otwarcia pliku do odczytu.
```PHP
$handle = fopen("nazwa_pliku.txt", "r");
```
Następnie użyjemy funkcji `fread()` do odczytania zawartości pliku i przypisania jej do zmiennej.
```PHP
$contents = fread($handle, filesize("nazwa_pliku.txt"));
```
Na koniec zamknijmy połączenie z plikiem za pomocą funkcji `fclose()`.
```PHP
fclose($handle);
```
### Przykładowy wynik

Bardzo ważne jest, aby upewnić się, że plik ma odpowiedni format i zawiera poprawne dane do odczytania. W przeciwnym razie, nie będzie możliwe prawidłowe odczytanie danych z pliku. Poniżej jest przykładowy wynik odczytania pliku CSV z danymi.

![przykładowy_wynik](https://user-images.githubusercontent.com/68320777/128888246-2107e730-61df-4cde-a196-01f3d8fcf3b7.png)

## Deep Dive

Funkcja `fopen()` pozwala określić tryb otwarcia pliku, w naszym przykładzie użyliśmy trybu `r` do odczytu. Istnieją również inne tryby dostępne, takie jak `w` do zapisu, `a` do dopisywania lub `x` do tworzenia nowego pliku. Więcej informacji na temat trybów można znaleźć w dokumentacji PHP.

Pamiętaj również o zamknięciu połączenia z plikiem za pomocą funkcji `fclose()`. Jest to ważne nie tylko dla bezpieczeństwa, ale także dla wydajności naszego kodu.

## Zobacz także
- [Dokumentacja PHP: fread()](https://www.php.net/manual/pl/function.fread.php)
- [Dokumentacja PHP: fopen()](https://www.php.net/manual/pl/function.fopen.php)
- [Dokumentacja PHP: fclose()](https://www.php.net/manual/pl/function.fclose.php)