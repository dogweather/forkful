---
title:                "Fish Shell: Praca z plikami csv"
simple_title:         "Praca z plikami csv"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/working-with-csv.md"
---

{{< edit_this_page >}}

## Dlaczego

CSV, czyli Comma-Separated Values, to popularny format plików tekstowych używany do przechowywania danych w tabelach. Jest to doskonałe narzędzie dla osób pracujących z dużymi ilościami danych, ponieważ jest czytelny dla ludzi i łatwy do przetwarzania przez komputery. W tym artykule dowiesz się, dlaczego warto nauczyć się programowania w Fish Shell właśnie dla obsługi plików CSV.

## Jak to zrobić

Fish Shell jest wszechstronnym narzędziem, które pozwala na sprawną obsługę plików CSV za pomocą kilku prostych poleceń. Poniżej przedstawiamy kilka przykładowych kodów, które pomogą Ci zacząć pracę z CSV w Fish Shell.

```Fish Shell
# Stworzenie nowego pliku CSV z nagłówkiem w pierwszej linii
echo "Imię,Nazwisko,Wiek" > dane.csv

# Dodanie nowych danych do pliku CSV
echo "Anna,Kowalska,32" >> dane.csv
echo "Jan,Nowak,42" >> dane.csv
echo "Karolina,Pawlak,28" >> dane.csv

# Zamiana separatora na średnik zamiast przecinka
sed 's/,/;/g' dane.csv > dane2.csv

# Wyświetlanie danych w tabeli
cat dane2.csv | csvlook

# Sortowanie danych po kolumnie "Wiek"
cat dane2.csv | csvsort -c Wiek | csvlook
```

Z powyższych przykładów możemy wywnioskować, że Fish Shell posiada proste i intuicyjne polecenia do obsługi plików CSV, dzięki czemu nie ma potrzeby korzystania z zewnętrznych aplikacji czy narzędzi.

## Głębsza analiza

Podczas pracy z CSV w Fish Shell, warto zwrócić uwagę na kilka istotnych aspektów. Przede wszystkim format pliku musi być zgodny z określonymi standardami, czyli nagłówkami i separatorami. W przypadku problemów z czytaniem lub zapisywaniem danych, warto sprawdzić te elementy.

Kolejną ważną kwestią jest parsowanie danych w celu przetworzenia ich na odpowiednie typy, np. liczbowe czy daty. W Fish Shell, można skorzystać z polecenia csvnormalize, które automatycznie przeprowadza ten proces.

Pamiętaj również o ochronie danych, szczególnie wrażliwych informacji, zawartych w plikach CSV. Fish Shell pozwala na szyfrowanie plików oraz dostosowanie uprawnień dostępu do nich.

## Zobacz również

- [Dokumentacja Fish Shell](https://fishshell.com/docs/current/)
- [Oficjalny poradnik CSV w Fish Shell](https://fishshell.com/docs/current/tutorial.html#tutorial-csv)
- [Polecenia do obsługi CSV w Fish Shell](https://fishshell.com/docs/3.3/cmds/csv.html)