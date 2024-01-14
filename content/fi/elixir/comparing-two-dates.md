---
title:                "Elixir: Kahden päivämäärän vertaaminen"
programming_language: "Elixir"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Miksi vertailla kahta päivämäärää?
Vertaileminen on tärkeä osa ohjelmointia ja voi auttaa meitä ymmärtämään tapahtumien järjestystä tai määrittämään tietyn ajanjakson kulkemista. Käyttäen Elixirin Date-moodulia, voimme helposti vertailla kahta päivämäärää ja saada selville niiden suhteen toisiinsa. Tässä blogikirjoituksessa opimme miten se tehdään.

## Miten se tehdään
MySQL-tietokannasta haetaan kaksi päivämäärää ja tallennetaan ne taulukkoon "dates". Sitten käytämme Elixirin Date-moodulia vertailemaan näitä kahta päivämäärää.

```
# Elixir-koodiesimerkki:
dates = [date1: ~D[2021-01-01], date2: ~D[2021-03-15]]
date1_after_date2 = Date.compare(dates[:date1], dates[:date2])
date2_after_date1 = Date.compare(dates[:date2], dates[:date1])

# Tulos:
date1_after_date2 = :greater
date2_after_date1 = :less
```

Voit myös käyttää Date.compare-tietokantatoimintoa suoraan kahden tietokantapäivämäärän välillä:

```
# Elixir-koodiesimerkki:
date1 = Ecto.DateTime.cast(timestamp1)
date2 = Ecto.DateTime.cast(timestamp2)
comparison = Date.compare(date1, date2)

# Tulos:
comparison = :equal
```

## Syvemmälle aiheeseen
Elixirin Date-mooduli sisältää useita vertailuun liittyviä toimintoja. Voit käyttää Date.same?-toimintoa selvittääksesi ovatko kaksi päivämäärää samat ja Date.between?-toimintoa selvittääksesi, että onko tietty päivämäärä kahden annetun päivämäärän välissä. Voit myös käyttää Date.before?- ja Date.after?-toimintoja selvittääksesi mikä päivämäärä sijoittuu aikaisemmin tai myöhemmin kahdesta annetusta päivämäärästä.

Voit myös käyttää Date.diff-toimintoa saadaksesi tiedon kahden päivämäärän välisestä erosta päivinä, kuukausissa tai vuosissa. Tarkempien tietojen saamiseksi voit käyttää Date.diff-tietokantatoimintoa, joka palauttaa eron tietokantamuodossa.

## Katso myös
- Elixirin Date-moodulin dokumentaatio: https://hexdocs.pm/elixir/Date.html
- Elixirin vertailutoiminnot: https://hexdocs.pm/elixir/master/comparisons.html
- Elixir School: Elixirin opiskeluun tarkoitettu verkkosivusto: https://elixirschool.com/fi/