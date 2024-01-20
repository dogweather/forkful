---
title:                "Kahden päivämäärän vertaaminen"
html_title:           "Bash: Kahden päivämäärän vertaaminen"
simple_title:         "Kahden päivämäärän vertaaminen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Vertailemme kahta päivämäärää ymmärtääksemme niiden ajallista suhdetta. Ohjelmoijat tekevät tämän useimmiten päättääkseen, mitä seuraavaksi suoritetaan.

## Kuinka Tehdä:
Seuraavassa nähdään, kuinka kahta päivämäärää verrataan Elixir-ohjelmointikielessä.

```elixir
dt1 = Date.new!(2021, 12, 1)
dt2 = Date.new!(2021, 12, 2)

if Date.compare(dt1, dt2) == :lt do
  IO.puts "Päivämäärä dt1 on ennen päivämäärää dt2"
else
  IO.puts "Päivämäärä dt1 ei ole ennen päivämäärää dt2"
end
```
Tämä antaa tulosteen:
```elixir
"Päivämäärä dt1 on ennen päivämäärää dt2"
```
Date.compare palauttaa: :gt, :lt, tai :eq jos dt1 on suurempi, pienempi tai yhtä suuri kuin dt2.

## Syvemmälle:
Elixirin päivämäärien vertailu periytyy Erlang-kielestä, jonka pohjalta Elixir luotiin. `Date.compare` -funktio on nopea ja tehokas tapa verrata päivämääriä. Vaihtoehtoisesti voidaan käyttää `Date.diff`, joka palauttaa päivien eron kahteen päivään. Implementointidetaljina, päivämäärät tallennetaan sisäisesti kokonaislukutietotyyppeinä, mikä mahdollistaa nopean vertailun.

## Katso Myös:
- Elixirin virallinen dokumentaatio: [Date](https://hexdocs.pm/elixir/Date.html)
- Erlangin päivämäärien käsittely - [Calendar](http://erlang.org/doc/man/calendar.html)
- Great article on Elixir Date & Time handling - [Working with dates and times in Elixir](https://www.amberbit.com/blog/2019/4/25/dates-and-times-in-elixir/)