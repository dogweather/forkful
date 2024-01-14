---
title:                "Ruby: Pobieranie aktualnej daty"
simple_title:         "Pobieranie aktualnej daty"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego

Zastanawiałeś się kiedykolwiek, jak w prosty sposób sprawić, żeby Twój program pokazał aktualną datę? W tym artykule porozmawiamy o tym, dlaczego warto nauczyć się pobierać bieżącą datę w języku Ruby.

## Jak to zrobić

Istnieje kilka sposobów na pobranie bieżącej daty w Ruby. Niezależnie od tego, czy używasz Ruby w trybie interaktywnym, czy piszesz skrypt, zamieścimy tutaj kilka praktycznych przykładów.

```Ruby
# Pobieranie daty i czasu w jednej zmiennej
current_time = Time.now
puts current_time

# Pobieranie tylko daty
current_date = Date.today
puts current_date

# Formatowanie wyjścia
puts current_time.strftime("%d/%m/%Y") # wyświetli datę w formacie DD/MM/YYYY
puts current_date.strftime("%B %Y") # wyświetli datę w formacie miesiąc/YEAR
```

Output dla powyższego kodu będzie wyglądał mniej więcej tak:

```
2021-09-01 09:00:00 +0200
2021-09-01
01/09/2021
September 2021
```

W powyższym przykładzie użyliśmy metody `now` z klasy `Time` oraz `today` z klasy `Date`. Należy zauważyć, że obie te klasy są wbudowane w język Ruby, więc nie musimy importować żadnych dodatkowych bibliotek.

## Dogłębne zagłębienie

Zanim przejdziemy do dogłębnego wyjaśnienia, warto wspomnieć, że w Ruby istnieje również klasa `DateTime`, która łączy zalety klas `Time` i `Date` - można w niej przechowywać zarówno datę, jak i czas. Możesz się z nią zapoznać w swoim wolnym czasie.

A teraz kilka słów o tym, jak dokładnie działa metoda `now`. W rzeczywistości, `now` ma dokładnie ten sam efekt, co wywołanie `Time.new` - jest to skrótowe wyrażenie. Metoda ta zwraca obiekt `Time` z aktualnym czasem, czyli datą i godziną. Należy jednak pamiętać, że obiekt ten będzie aktualny tylko w momencie jego utworzenia - jeśli będziemy go przechowywać w zmiennej i wykorzystywać później, to jego wartość nie będzie już aktualna.

Kolejną ciekawostką jest fakt, że klasa `Time` przechowuje czas w formacie UTC (czas uniwersalny), a nie w naszym lokalnym strefie czasowej. Dlatego też, jeśli chcemy wyświetlić aktualny czas w naszej strefie, musimy podać odpowiednią offsetową wartość do metody `strftime`.

## Zobacz także

Jeśli sądzisz, że pobieranie daty w Ruby jest fascynujące, to koniecznie sprawdź te artykuły:

- [Manipulowanie datami w Ruby](https://www.rubyguides.com/2015/12/ruby-date/)
- [Różnica między klasami Date, Time i DateTime](https://blog.appsignal.com/2020/07/22/time-objects-in-ruby.html)
- [Przydatne metody klasy Date](https://www.vojtechruzicka.com/ruby-date-class/)