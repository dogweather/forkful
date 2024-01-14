---
title:    "Python: Konwertowanie daty na ciąg znaków"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Dlaczego warto konwertować datę na ciąg znaków

Konwertowanie daty na ciąg znaków jest ważnym aspektem programowania w Pythonie, ponieważ pozwala nam wygodnie wyświetlać informacje o dacie w formie zrozumiałej dla użytkownika. Jest to szczególnie przydatne w przypadku tworzenia aplikacji, gdzie dane czasowe są ważnym elementem interakcji z użytkownikiem.

## Jak to zrobić

Konwersja daty na ciąg znaków jest prosta w Pythonie. Wystarczy użyć funkcji `strftime` i podać format, w jakim chcemy wyświetlić datę. Przykładowy kod:

```Python
import datetime

today = datetime.date.today()

print(today.strftime("Dziś jest %d.%m.%Y"))
```

Output:

```
Dziś jest 07.03.2021
```

W powyższym przykładzie użyliśmy funkcji `strftime`, aby wyświetlić dzisiejszą datę w formacie "Dziś jest dzień.miesiąc.rok". Możemy zmieniać formatowanie w dowolny sposób, wykorzystując odpowiednie symbole.

## Głębsza analiza

Konwertowanie daty na ciąg znaków jest możliwe dzięki modułowi `datetime` w Pythonie. Jest to moduł, który umożliwia operacje na czasie i dacie, w tym również konwertowanie jej na różne formaty.

Funkcja `strftime` akceptuje rożne symbole, które odpowiadają różnym elementom daty, takim jak dzień, miesiąc czy rok. Możemy również wyświetlać informacje o godzinie, minutach i sekundach. Szczegółowa lista symboli jest dostępna w dokumentacji Pythona.

## Zobacz także

- Dokumentacja Pythona dotycząca formatowania daty: https://docs.python.org/3/library/datetime.html#strftime-and-strptime-format-codes
- Poradnik dotyczący formatowania daty w Pythonie: https://realpython.com/python-datetime/
- Przykładowe zastosowania konwersji daty w aplikacjach webowych: https://www.geeksforgeeks.org/python-date-to-string/