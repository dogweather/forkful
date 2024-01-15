---
title:                "Praca z plikami csv"
html_title:           "Bash: Praca z plikami csv"
simple_title:         "Praca z plikami csv"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/working-with-csv.md"
---

{{< edit_this_page >}}

## Dlaczego

CSV (Comma-Separated Values) jest popularnym formatem do przechowywania danych w arkuszach kalkulacyjnych i bazach danych. W tym artykule dowiesz się, jak w łatwy sposób przetwarzać pliki CSV za pomocą języka Bash.

## Jak To Zrobić

Przetwarzanie plików CSV w Bash jest możliwe dzięki zastosowaniu pętli i poleceń takich jak "cut" czy "awk". Przykładowy plik CSV może wyglądać tak:

```Bash
Imię,Nazwisko,Wiek
Anna,Kowalska,30
Jan,Nowak,45
Karolina,Nowicka,25
```

Aby wypisać dane z tego pliku, użyjemy polecenia "cat" wraz ze znakiem przekierowania ">", które umożliwi zapisanie wyników do nowego pliku. Nasza pętla będzie przetwarzać każdą linię pliku, dzieląc ją na poszczególne pola i wyświetlając odpowiednie informacje.

```Bash
cat plik.csv | while read line
do
    imie=$(echo $line | cut -d ',' -f1)
    nazwisko=$(echo $line | cut -d ',' -f2)
    wiek=$(echo $line | cut -d ',' -f3)
    echo "Imię: $imie, Nazwisko: $nazwisko, Wiek: $wiek"
done > wynik.csv
```

W efekcie, powyższe polecenie utworzy nowy plik "wynik.csv" zawierający przetworzone dane, które wyglądają następująco:

```Bash
Imię: Anna, Nazwisko: Kowalska, Wiek: 30
Imię: Jan, Nazwisko: Nowak, Wiek: 45
Imię: Karolina, Nazwisko: Nowicka, Wiek: 25
```

## Pogłębiona Analiza 

Podczas przetwarzania plików CSV w Bash warto pamiętać o kilku ważnych aspektach. Znaki przecinka (",") występujące w tekście mogą zakłócać poprawne przetwarzanie, dlatego warto używać opcji "-d" w poleceniu "cut", aby określić podział pól za pomocą innego znaku. W przypadku gdy nasz plik CSV zawiera nagłówki, możemy dodać opcję "tail -n +2" aby pominąć pierwszą linię podczas przetwarzania.

W przypadku, gdy mamy do czynienia z większą ilością danych, warto skorzystać z polecenia "sed" w celu filtrowania lub dodawania danych do pliku CSV, a następnie przetwarzać go za pomocą pętli i poleceń "grep" lub "awk" w celu uzyskania pożądanych informacji.

## Zobacz również

- [Dokumentacja Bash](https://www.gnu.org/software/bash/)
- [Poradnik Bash - Przetwarzanie plików CSV] (https://linuxhint.com/parse_csv_bash/)
- [10 Przydatnych Przykładów Przetwarzania Plików CSV w Bash] (https://likegeeks.com/working-with-csv-files-in-bash/)