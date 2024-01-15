---
title:                "Päivämäärän muuttaminen merkkijonoksi"
html_title:           "Elixir: Päivämäärän muuttaminen merkkijonoksi"
simple_title:         "Päivämäärän muuttaminen merkkijonoksi"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi
Kuvittele, että sinulla on ohjelma, jossa haluat näyttää päivämäärän käyttäjälle ymmärrettävässä muodossa, esimerkiksi "14. tammikuuta 2021". Tässä tilanteessa parseeraamalla ja muuntamalla päivämäärän stringiksi voit helposti toteuttaa halutun toiminnallisuuden.

## Kuinka
Käytä Elixirin `Date`-moduulia löytääksesi tarvittavan toiminnon. Voit ensin luoda päivämäärä-olion antamalla sille vuoden, kuukauden ja päivän sisältävät numerot:

```Elixir
date = Date.new(2021, 1, 14)
```

Tämän jälkeen voit käyttää `Date`-moduulin `format`-funktiota määrittelemällä halutun formaatin merkkinä:

```Elixir
string_date = Date.format(date, "%e. %B %Y") # 14. tammikuuta 2021
```

Muita hyödyllisiä formaattivaihtoehtoja ovat esimerkiksi `%d.%m.%Y` (14.01.2021) ja `%A, %d.%m.%Y` (torstai, 14.01.2021). Voit löytää lisää vaihtoehtoja Elixirin [Dokumentaatiosta](https://hexdocs.pm/elixir/Date.html#module-formatting-and-parsing).

## Syvenny
`Date`-moduulin takana oleva toiminnallisuus perustuu Erlangin `calendar`-moduuliin, joka mahdollistaa päivämäärä- ja aikatoimintojen käytön Elixirissä. Tämä mahdollistaa myös kansainväliset päivämäärämuodot, kuten esimerkiksi `Date.format(date, "%e %B, %Y", :en)`, joka tulostaa päivämäärän englanniksi (14 January 2021).

## Katso myös
- [Date-moduulin Dokumentaatio](https://hexdocs.pm/elixir/Date.html)
- [Elixirin Tietoja ja Opastusta -sivusto](https://elixir-lang.org/)