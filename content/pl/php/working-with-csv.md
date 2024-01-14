---
title:                "PHP: Praca z plikami csv"
simple_title:         "Praca z plikami csv"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/working-with-csv.md"
---

{{< edit_this_page >}}

## Dlaczego

Praca z plikami CSV jest nieodłączną częścią programowania w PHP. Są one nie tylko powszechnie używane do przechowywania danych, ale również prostsze w obsłudze niż tradycyjne bazy danych. Pozwól mi pokazać, jak szybko i łatwo można pracować z CSV przy użyciu PHP.

## Jak to zrobić

Przede wszystkim musimy pobrać plik CSV przy użyciu funkcji `fopen()`. Następnie możemy użyć pętli `while` do odczytywania danych z pliku wiersz po wierszu. Tutaj przykładowy kod:

```PHP
$file = fopen("plik.csv", "r");

// odczytywanie danych z pliku
while (($data = fgetcsv($file)) !== FALSE) {
    // wyświetlanie danych w konsoli
    print_r($data);
}

fclose($file);
```

W powyższym przykładzie użyliśmy funkcji `fgetcsv()`, która odczytuje wiersz z pliku i zwraca go jako tablicę. Możemy również użyć funkcji `fputcsv()`, aby zapisać dane do pliku CSV.

Możemy także wyświetlić dane z pliku CSV w tabeli HTML. Oto przykładowy kod:

```PHP
$file = fopen("plik.csv", "r");

echo "<table>";
while (($data = fgetcsv($file)) !== FALSE) {
    echo "<tr>";
    foreach ($data as $value) {
        echo "<td>" . $value . "</td>";
    }
    echo "</tr>";
}
fclose($file);
echo "</table>";
```

## Głębszy zanurzenie

W przypadku dużych plików CSV, obiekt `SplFileObject` może być przydatniejszy. Pozwala to na iterowanie po wierszach za pomocą pętli `foreach`, a także na używanie metod do odczytywania lub zapisywania danych. Oto przykładowy kod:

```PHP
$file = new SplFileObject("plik.csv");

// odczytywanie danych z pliku
foreach ($file as $row) {
    // wyświetlanie danych w konsoli
    print_r($row);
}

// zapisywanie danych do pliku
$file->fputcsv(["Nowe dane", "do zapisania"]);
```

Inną ważną rzeczą do zapamiętania jest fakt, że pliki CSV mogą mieć różne ograniczenia, takie jak różne znaki oddzielające dane lub różne znaki jako nagłówki kolumn. Dlatego dobrze jest używać funkcji `fgetcsv()` z odpowiednimi ustawieniami lub bibliotek, takich jak `League/CSV`, które łatwo radzą sobie z różnymi formatami plików CSV.

## Zobacz także

- [Dokumentacja PHP dotycząca pracy z plikami CSV](https://www.php.net/manual/en/function.fgetcsv.php)
- [Biblioteka League/CSV](https://csv.thephpleague.com/)

Dzięki temu szybko i łatwo będziesz mógł pracować z plikami CSV w swoich projektach w PHP. Zwłaszcza przy użyciu odpowiednich bibliotek, możesz uniknąć wielu problemów związanych z różnymi formatami plików. Powodzenia!