---
title:                "Praca z plikami csv"
html_title:           "PHP: Praca z plikami csv"
simple_title:         "Praca z plikami csv"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/working-with-csv.md"
---

{{< edit_this_page >}}

## Dlaczego

Praca z plikami CSV (Comma-Separated Values) jest powszechnym zadaniem dla programistów PHP, ponieważ pozwala na łatwe przetwarzanie i manipulowanie dużymi zbiorami danych. Jest to szczególnie przydatne w kontekście analizy danych, eksportowania i importowania informacji oraz tworzenia raportów. W tym artykule dowiesz się, dlaczego warto poznać podstawy tej techniki i jak zacząć pracę z plikami CSV w PHP.

## Jak to zrobić

Zacznijmy od wyjaśnienia, czym jest plik CSV. Jest to plik tekstowy, w którym dane są przechowywane w postaci tablicy, z wartościami oddzielonymi przecinkami lub innymi specjalnie zdefiniowanymi znakami, takimi jak średnik czy tabulator. Aby zacząć pracę z plikiem CSV w PHP, musisz najpierw użyć funkcji `fopen()` aby go otworzyć:

```PHP
$plik = fopen('dane.csv', 'r');
```

W powyższym przykładzie używamy `fopen()` z dwoma argumentami - nazwę pliku (w tym przypadku `dane.csv`) oraz tryb, w jakim chcemy otworzyć plik (`r` oznacza tryb tylko do odczytu). Teraz, gdy mamy otwarty plik, możemy użyć funkcji `fgetcsv()` do wczytania danych:

```PHP
$wiersz = fgetcsv($plik);
```

Funkcja ta wczyta kolejny wiersz z pliku i zwróci go jako tablicę wartości. Możemy następnie użyć pętli `while` aby wczytywać kolejne wiersze, dopóki nie osiągniemy końca pliku:

```PHP
while (!feof($plik)) {
    $wiersz = fgetcsv($plik);
    // mamy dostęp do danych w $wiersz i możemy je dalej przetworzyć
}
```

W celu przetworzenia danych możemy użyć funkcji takich jak `explode()` aby podzielić wartości w wierszu na oddzielne elementy, lub `implode()` aby połączyć wartości w jedną ciągłą wartość. Następnie możemy wykorzystać te przetworzone dane do wyświetlenia w formie tabeli lub do zapisania w nowym pliku CSV.

## Wnikliwa analiza

Praca z plikami CSV w PHP może być dość prosta i intuicyjna, jednak istnieje kilka rzeczy, które warto wiedzieć, aby ułatwić sobie to zadanie. Przede wszystkim, aby uniknąć błędów w odczytywaniu danych, należy uważać na rodzaj separatora w pliku CSV. Jeśli może być on różny od standardowego przecinka, należy użyć opcji `delimiter` w funkcji `fgetcsv()` aby to ustalić.

Kolejną przydatną funkcją jest `fputcsv()`, która pozwala na zapisywanie danych do pliku CSV w łatwy sposób. Przykład użycia mógłby wyglądać następująco:

```PHP
$wiersz = array('John', 'Smith', 30);
fputcsv($plik, $wiersz);
```

Powyższy kod zapisze w pliku (który otworzyliśmy wcześniej) nowy wiersz z wartościami `John`, `Smith` i `30`, oddzielając je przecinkami.

Innym istotnym aspektem jest obsługa znaków specjalnych w pliku CSV. Jeśli dane zawierają znaki specjalne, takie jak cudzysłowy czy znaki nowej linii, należy odpowiednio poprzedzić je znakiem `\` aby uniknąć błędów przy odczytywaniu.

## Zobacz także

Jeśli chcesz pogłębić swoją wiedzę na