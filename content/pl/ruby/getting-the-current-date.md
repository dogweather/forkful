---
title:                "Ruby: Pobieranie aktualnej daty"
programming_language: "Ruby"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

Znajomość: Dlaczego korzystać z aktualnej daty w Ruby?

## Dlaczego

Korzystanie z aktualnej daty w programowaniu może być niezwykle pomocne w różnych przypadkach. Gdy tworzysz aplikację, która zajmuje się zarządzaniem zadaniami czy przypomnieniami, ważne jest, aby wyświetlać dokładną datę i czas, aby użytkownik miał pełną kontrolę nad swoimi wydarzeniami. Ponadto, jedną z najważniejszych funkcji przy tworzeniu aplikacji internetowych jest możliwość wyświetlania daty ostatniego zalogowania lub daty ostatniej aktualizacji danych. Dzięki temu użytkownicy mogą śledzić, jaką datą i o której godzinie ostatnio korzystali z aplikacji, co jest niezwykle przydatne w przypadku problemów technicznych lub w celu przeciwdziałania potencjalnym atakom hakerskim.

## Jak

Aby uzyskać aktualną datę w Ruby, możemy skorzystać z kilku różnych metod. W przypadku przechowywania daty w zmiennej, możemy użyć metody `Date.today` lub `Time.now`, aby przypisać do niej bieżącą datę. W celu wyświetlenia daty w konsoli, możemy wykorzystać metodę `puts`, która wyświetli nam bieżącą datę i czas.

```Ruby
today = Date.today
puts today # 2021-08-11

now = Time.now
puts now # 2021-08-11 12:34:56 +0200
```

Możemy również sformatować wyświetlanie daty przy pomocy metody `strftime`, która pozwala nam ustawić odpowiedni format. Na przykład, jeśli chcemy wyświetlić datę w formacie `dd.mm.yyyy`, możemy wykorzystać poniższy kod:

```Ruby
today = Date.today
puts today.strftime('%d.%m.%Y') # 11.08.2021
```

## Głębsza analiza

W Ruby istnieje również klasa `DateTime`, która jest połączeniem klasy `Date` i `Time` i pozwala nam zarówno na przechowywanie, jak i wyświetlanie aktualnej daty. Dodatkowo, w Ruby istnieje wiele innych metod, takich jak `prev_day`, `next_day`, `prev_month`, `next_month`, pozwalających na wygodne manipulowanie datami.

## Zobacz także

- Przewodnik po klasie Date w Ruby: https://docs.ruby-lang.org/en/2.0.0/Date.html
- Klasa Time w Ruby: https://docs.ruby-lang.org/en/2.0.0/Time.html
- Formatowanie daty w Ruby: https://www.techotopia.com/index.php/Ruby_Tutorial_-_Working_with_Dates_and_Times#Formatting_a_Date_and_Time_String