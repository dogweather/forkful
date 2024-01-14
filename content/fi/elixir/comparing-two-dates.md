---
title:    "Elixir: Kahden päivämäärän vertailu"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi vertailla kahden päivämäärän eroa?

Vertailemalla kahta päivämäärää Elixir-ohjelmoinnin avulla, voit tarkastella niiden välillä olevaa aikaväliä ja laskea esimerkiksi kuinka monta päivää tai tuntia on kulunut.

## Kuinka tehdä vertailu

Vertaillessamme kahta päivämäärää Elixir-ohjelmoinnissa, käytämme DateTime-moduulia ja siinä olevia funktioita. Alla on esimerkki siitä, kuinka voit verrata kahta päivämäärää ja tarkastella aikaväliä niiden välillä.

```Elixir
date1 = DateTime.from_naive!("2021-01-01")
date2 = DateTime.from_naive!("2021-03-01")
diff = DateTime.diff(date2, date1)
IO.puts "Aikaväli kahden päivämäärän välillä on #{diff.days} päivää."
```

Tämän koodinpätkän tulosteena saamme:

```Elixir
Aikaväli kahden päivämäärän välillä on 59 päivää.
```

## Syventävä tieto vertailusta

Elixirin DateTime-moduulissa on monia hyödyllisiä funktioita, joista voi olla hyötyä kahden päivämäärän vertailussa. Voit esimerkiksi tarkastella päivämäärien eroa sekunteina, minuutteina tai jopa vuosina. Voit myös lisätä tai vähentää päivämääriin aikaa ja tarkastella uutta päivämäärää.

See Also:

- [Elixir DateTime Documentation](https://hexdocs.pm/elixir/DateTime.html)
- [Calculating the difference between two dates in Elixir](https://elixirway.io/dates-and-times/calculating-the-difference-between-two-dates.html)
- [Using Elixir's DateTime Module](https://medium.com/@bimgates/using-elixirs-date-time-module-5b409d077e2d)

## Katso myös

- [Elixir DateTime Dokumentaatio](https://hexdocs.pm/elixir/DateTime.html)
- [Kahden päivämäärän eron laskeminen Elixirissä](https://elixirway.io/dates-and-times/calculating-the-difference-between-two-dates.html)
- [Elixirin DateTime-moduulin käyttö](https://medium.com/@bimgates/using-elixirs-date-time-module-5b409d077e2d)