---
title:                "PHP: Pobieranie aktualnej daty"
simple_title:         "Pobieranie aktualnej daty"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Dlaczego potrzebujemy aktualnej daty?

Aktualna data jest bardzo ważna w programowaniu. Pozwala nam na śledzenie czasu wykonywania naszych skryptów oraz na wyświetlanie aktualnej daty w aplikacjach. Ponadto, wiele funkcji programistycznych wymaga dostępu do bieżącego czasu, aby wykonać swoje zadania. Dlatego, dowiadując się jak pobrać aktualną datę w PHP, możemy ulepszyć nasze umiejętności programistyczne.

# Jak pobrać aktualną datę w PHP?

Aby pobrać aktualną datę w PHP, musimy użyć funkcji `date()`. Jest to wbudowana funkcja w PHP, która zwraca bieżącą datę i czas zgodnie z wybranym formatem.

### Przykład:
```PHP
$date = date("Y-m-d");
echo $date;
```

### Output:
```
2021-03-25
```

Funkcja `date()` pobiera dwa argumenty: format daty i opcjonalnie, timestamp. Jeśli nie podamy drugiego argumentu, funkcja automatycznie pobierze bieżącą datę i czas. W powyższym przykładzie, użyliśmy formatu `Y-m-d`, który zwraca rok, miesiąc i dzień.

Możemy również użyć innych specjalnych znaków formatujących, aby wyświetlić bardziej szczegółowe informacje o dacie i czasie. Na przykład, `d` oznacza dzień, `m` miesiąc, `Y` rok, a `H:i:s` godzina:minuta:sekunda.

# Dogłębny przegląd

W funkcji `date()` możemy również wybrać inny argument, timestamp, aby wyświetlić datę i czas w momencie innym niż bieżący. Timestamp jest liczbą reprezentującą ilość sekund, która upłynęła od 1 stycznia 1970 roku. Możemy go uzyskać używając funkcji `time()`.

### Przykład:
```PHP
$date = date("Y-m-d H:i:s", time() - 3600); //pobiera datę i czas z godzinę wcześniej
echo $date;
```

### Output:
```
2021-03-25 16:30:00
```

Możemy również wyświetlić datę i czas w różnych strefach czasowych, ustawiając odpowiednią strefę czasową w funkcji `date_default_timezone_set()`. Dzięki temu, możemy dostosować wyświetlanie daty i czasu do potrzeb naszej aplikacji.

# Zobacz również

Jeśli chcesz dowiedzieć się więcej o funkcji `date()` w PHP, warto zajrzeć na te strony:

- [Oficjalna dokumentacja PHP o funkcji date()](https://www.php.net/manual/en/function.date.php)
- [W3Schools - Tutorial on PHP Date and Time](https://www.w3schools.com/php/php_date.asp)
- [PHP Date Time Functions - GeeksforGeeks](https://www.geeksforgeeks.org/php-date-time-functions/)

Dziękujemy za przeczytanie naszego wpisu na temat pobierania aktualnej daty w PHP. Mamy nadzieję, że teraz wiesz, jak wykorzystać tę funkcję w swoim kodzie i zwiększyć swoje umiejętności programistyczne. Do zobaczenia w kolejnych artykułach!