---
title:                "Bash: Praca z plikami csv"
simple_title:         "Praca z plikami csv"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/working-with-csv.md"
---

{{< edit_this_page >}}

## Dlaczego

CSV (ang. Comma-Separated Values) to format przechowywania danych w postaci tabeli, gdzie wartości są oddzielone przecinkami. W dzisiejszych czasach sporo aplikacji i stron internetowych używa tego formatu do przechowywania danych, więc warto umieć z nim pracować. W tym poście pokażemy Ci, jak w łatwy sposób wykorzystać Bash do pracy z plikami CSV.

## Jak to zrobić

Bash to popularny język programowania, a zarazem potężne narzędzie do automatyzacji zadań systemowych. Dzięki prostocie i wszechstronności, Bash jest idealny do pracy z plikami CSV. Poniżej przedstawimy Ci kilka przykładów kodu, których pomocą będziesz mógł łatwo manipulować danymi w formacie CSV.

```Bash
# Wyświetlenie zawartości pliku CSV
cat plik.csv

# Wyświetlenie numeru wiersza w pliku
cat plik.csv | grep "nazwa_kolumny" | awk '{print NR}'

# Wyświetlenie wartości w wybranej kolumnie
cat plik.csv | cut -d',' -f2
```

Wykorzystując te i inne polecenia, możesz łatwo filtrować, przetwarzać i wyświetlać dane w pliku CSV. Zobaczysz też, jak przydatne jest wykorzystanie znaków specjalnych takich jak "," czy "|", aby dokładnie określić, jakie informacje chcesz wyświetlić.

## Deep Dive

Pliki CSV to nie tylko tabele z wartościami oddzielonymi przecinkami - mogą również zawierać wartości w innych formatach, jak na przykład daty czy liczby zmiennoprzecinkowe. Wskazówką może być używanie dostępnych już narzędzi do przetwarzania danych, takich jak `cut`, `grep` czy `awk`.

Pamiętaj też o tym, że pliki CSV mogą mieć różną strukturę, np. w niektórych przypadkach pierwszy wiersz zawiera nazwy kolumn, a w innych - nie. Dlatego ważne jest, aby sprawdzić dokładnie strukturę pliku przed przetwarzaniem.

## Zobacz również

Jeśli chcesz poznać więcej trików do pracy z plikami CSV w Bash, polecamy przeczytać artykuły na poniższych stronach:

- ["Manipulowanie danymi w formacie CSV z wykorzystaniem Bash"](https://www.lifewire.com/reading-and-processing-csv-files-in-bash-818549)
- ["Polecenia Shell do przetwarzania danych CSV"](https://www.geeksforgeeks.org/commands-used-in-processiong-of-generic-text-csv-files/)
- ["Szybki start dla pracy z plikami CSV w Bash"](https://linuxconfig.org/csv-manipulation-with-bash-using-csvkit)

Pozostaje nam tylko życzyć powodzenia w pracy z plikami CSV w Bash!