---
title:                "Praca z plikami CSV"
html_title:           "Bash: Praca z plikami CSV"
simple_title:         "Praca z plikami CSV"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/working-with-csv.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Praca z plikami CSV to manipulacja danymi w formacie "Comma-Separated Values" - prostym i uniwersalnym, dlatego często wybieranym do przechowywania i wymiany danych między różnymi systemami.

## Jak to zrobić?

```Bash
# Wczytanie pliku CSV i wyświetlenie zawartości
cat dane.csv

# Filtrowanie danych z pliku CSV: wybieranie wierszy z wartością większą niż 100 w drugiej kolumnie
awk -F, '$2 > 100' dane.csv

# Sortowanie pliku CSV po pierwszej kolumnie, numerycznie
sort -t, -k1,1n dane.csv

# Zliczanie ile razy pojawiła się unikalna wartość w pierwszej kolumnie
cut -d, -f1 dane.csv | sort | uniq -c

# Podmienianie przecinków na średniki, zapisać do nowego pliku
sed 's/,/;/g' dane.csv > dane_semicolon.csv
```

Przykładowe wyjście dla komendy `cat dane.csv`:
```
Jan,Kowalski,30
Anna,Nowak,45
```

## W głębi tematu

Format CSV wywodzi się z lat 70., kiedy to po raz pierwszy zaczęto go używać w programach komputerowych do przechowywania danych tabelarycznych. Alternatywy dla CSV to m.in. JSON, XML, YAML, które mogą być bardziej elastyczne pod względem struktury danych, jednak CSV nadal pozostaje popularne ze względu na prostotę i szeroką kompatybilność. Ważne jest, aby pamiętać o odpowiednim cytowaniu danych, jeśli w wartościach występują przecinki, oraz o ujednoliceniu kodowania znaków, szczególnie podczas pracy na różnych systemach.

## Zobacz też

- [GNU Awk User's Guide](https://www.gnu.org/software/gawk/manual/) – dokumentacja składni i używania `awk`.
- [GNU sed manual](https://www.gnu.org/software/sed/manual/sed.html) – dokumentacja `sed` i wyrażeń regularnych.
- [Bash Guide for Beginners](https://tldp.org/LDP/Bash-Beginners-Guide/html/) – przewodnik po podstawach skryptów w Bashu.