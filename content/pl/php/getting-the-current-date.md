---
title:                "Uzyskiwanie bieżącej daty"
html_title:           "PHP: Uzyskiwanie bieżącej daty"
simple_title:         "Uzyskiwanie bieżącej daty"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego

Dlaczego ktoś mógłby chcieć poznać bieżącą datę? Cóż, pewnie nie ma nic bardziej frustrującego niż musieć ręcznie wprowadzać datę w aplikacji lub formularzu internetowym. Dlatego warto nauczyć się pobierać bieżącą datę za pomocą PHP, aby ułatwić sobie życie i oszczędzić czas.

## Jak to zrobić

Pierwszym krokiem do uzyskania bieżącej daty jest użycie funkcji `date()` w PHP. Przyjmuje ona dwa argumenty - format daty i opcjonalnie timestamp. Jeśli nie podamy drugiego argumentu, zostanie użyty bieżący czas.

```PHP
$currentDate = date("d-m-Y"); // zwróci bieżącą datę w formacie d-m-Y 
```

Możemy również wyświetlić bieżący czas, dodając godzinę i minutę w formacie 24-godzinnym.

```PHP
$currentDate = date("H:i:s"); // zwróci bieżący czas w formacie H:i:s 
```

Możemy także wykorzystać funkcję `time()` do pobrania czasu w sekundach od 1 stycznia 1970 roku i przekazać ten wynik jako drugi argument do funkcji `date()`.

```PHP
$currentDate = date("d-m-Y H:i:s", time()); // zwróci bieżącą datę i czas w formacie d-m-Y H:i:s 
```

Możemy również wykorzystać funkcję `strtotime()` do przekształcenia daty w formacie tekstowym na timestamp, który potem możemy wykorzystać w funkcji `date()`.

```PHP
$currentDate = date("d-m-Y H:i:s", strtotime("now")); // zwróci bieżącą datę i czas w formacie d-m-Y H:i:s
```

Warto pamiętać, że funkcje `date()` i `time()` uwzględniają strefę czasową ustawioną na serwerze, więc wynik może się różnić w zależności od ustawień.

## Deep Dive

Jeśli potrzebujemy bardziej szczegółowej informacji na temat daty, możemy wykorzystać funkcję `getdate()`, która zwróci nam bieżącą datę w postaci tablicy asocjacyjnej. W tej tablicy znajdziemy m.in. dzień tygodnia, dzień miesiąca, miesiąc, rok, godzinę, minutę, sekundę oraz wiele innych informacji.

```PHP
$currentDate = getdate(); // zwróci bieżącą datę w postaci tablicy asocjacyjnej 
```

Możemy także wyświetlić tylko wybraną wartość zwróconej tablicy, na przykład godzinę.

```PHP
$hour = $currentDate["hours"]; // zwróci bieżącą godzinę 
```

Funkcja `getdate()` jest szczególnie przydatna, gdy potrzebujemy uzyskać szczegółowe informacje na temat bieżącej daty.

## Zobacz także

- [Dokumentacja PHP na temat funkcji date()](https://www.php.net/manual/en/function.date.php)
- [Informacje na temat funkcji getdate()](https://www.php.net/manual/en/function.getdate.php)
- [Przykłady użycia funkcji strtotime()](https://www.php.net/manual/en/function.strtotime.php)