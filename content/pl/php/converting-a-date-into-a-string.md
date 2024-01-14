---
title:                "PHP: Konwersja daty na łańcuch znaków"
programming_language: "PHP"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Wiele osób ma potrzebę przekształcenia daty na ciąg znaków w trakcie programowania. Może to być wymagane, na przykład, do wyświetlenia daty w formacie zrozumiałym dla użytkownika lub do wprowadzenia daty w odpowiednim formacie do bazy danych. W tym artykule przedstawię Wam kilka sposobów na przekształcenie daty w ciąg znaków za pomocą PHP.

## Jak to zrobić

#### Metoda 1: Używanie funkcji `date()`

```PHP
// definicja zmiennej z datą
$date = date('Y-m-d H:i:s');
// wyświetlenie daty w formacie rrrr-mm-dd HH:MM:SS
echo $date;
// output: 2021-02-28 13:30:45
```

#### Metoda 2: Używanie funkcji `strftime()`

```PHP
// definicja zmiennej z datą
$date = time();
// wyświetlenie daty w formacie DD-MMM-RRRR
echo strftime('%d-%b-%Y', $date);
// output: 28-Feb-2021
```

#### Metoda 3: Używanie obiektu `DateTime`

```PHP
// definicja obiektu DateTime
$date = new DateTime('2021-02-28');
// wyświetlenie daty w formacie DD, MM RRRR
echo $date->format('d, M Y');
// output: 28, Feb 2021
```

## Deep Dive

W powyższych przykładach użyliśmy różnych metod, aby przekształcić datę w ciąg znaków. Funkcja `date()` jest najprostszym sposobem na wyświetlenie aktualnej daty, jednak nie zapewnia ona wielu opcji formatowania. Funkcja `strftime()` jest bardziej zaawansowana, pozwala na wyświetlenie daty w różnych językach, jednak wymaga użycia odpowiednich znaków formatujących. Ostatnia metoda z użyciem obiektu `DateTime` jest najbardziej elastyczna, ponieważ pozwala na dostosowanie daty do naszych potrzeb za pomocą różnych metod i formatów.

W przypadku bardziej skomplikowanych operacji na datach, zaleca się zapoznanie się z dokumentacją PHP dotyczącą dat i czasu oraz z bibliotekami zewnętrznymi, takimi jak Carbon, która ułatwia manipulowanie datami i czasem.

## Zobacz także

- [Oficjalna dokumentacja PHP - Daty i Czas](https://www.php.net/manual/pl/datetime.html)
- [Biblioteka Carbon](https://carbon.nesbot.com/)
- [Porównanie różnych sposobów formatowania daty w PHP](https://www.inanimateobjects.com/2012/02/06/difference-between-phps-date-function-and-strftime-function/)