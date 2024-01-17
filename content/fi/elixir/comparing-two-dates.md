---
title:                "Kahden päivämäärän vertailu"
html_title:           "Elixir: Kahden päivämäärän vertailu"
simple_title:         "Kahden päivämäärän vertailu"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Päivämäärien vertailu on prosessi, jossa ohjelmoijat vertaavat kahta eri päivämäärää ja tarkistavat, ovatko ne samat, eri vai kumpi on suurempi. Tämä on tärkeätä esimerkiksi tietokannoissa, kun etsitään tietoja tietyltä ajanjaksolta tai järjestetään tuloksia aikajärjestykseen.

## Miten:

```Elixir
Date.compare({{2020, 12, 1}, {2020, 12, 2}}) 
```

Tuloksena on:

```
:lt
```

Suurin osa Elixirin päivämäärätoiminnoista käyttää Erlangin kalenterimoduulia, joka käyttää Gregoriaanista kalenteria ja tallentaa päivämäärän tietueena {vuosi, kuukausi, päivä}.

## Syvemmälle:

Päivämäärien vertailu on ollut haastavaa monille ohjelmointikielille, mutta Elixirin tapauksessa se on helppoa ja tehokasta Erlangin kalenterimoduulin ansiosta. Lisäksi voit käyttää myös Elixirin date-moduulia, joka tuo lisää toimintoja päivämäärien käsittelyyn.

## Katso myös:

[Lisätietoa Elixirin päivämäärätoiminnoista](https://hexdocs.pm/elixir/master/Date.html)

[Turvallisempi tapa vertailla päivämääriä](https://dev.to/rizafahmi/comparing-dates-in-elixir-3la4)