---
title:    "Gleam: Päivämäärän muuntaminen merkkijonoksi"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Miksi

Monissa ohjelmointitehtävissä päivämäärän muuttaminen merkkijonoksi on välttämätöntä, jotta voimme näyttää päivämäärän tietokannasta tai käyttäjältä saadusta syötteestä halutussa muodossa. Tämä on myös hyödyllistä esimerkiksi raportoinnissa tai verkkosovelluksissa, joissa on tarvetta näyttää päivämäärät suurelle yleisölle.

## Miten tehdä

Aloittaaksemme päivämäärän muuttamisen merkkijonoksi Gleamissa, meidän täytyy ensin tuoda käyttöömme `Date`-moduuli. Tämän jälkeen voimme käyttää `format`-funktiota muuttaaksemme päivämäärän haluamaamme muotoon:

```Gleam
import Date

let date = Date.build(2021, 12, 31)
let formatted_date = Date.format(date, "%d.%m.%Y")
```

Tässä esimerkissä me tuomme `Date`-moduulin, joka tarjoaa meille valmiin `build`-funktion luodaksemme uuden päivämäärä-olion. Sen jälkeen käytämme `format`-funktiota, joka ottaa ensimmäisenä parametrinaan päivämäärä-olion ja toisena parametrinaan merkkijonon, joka määrittelee halutun muodon. Tässä tapauksessa meidän muodoksemme tulee `"%d.%m.%Y"`, joka tarkoittaa päivä.kuukausi.vuosi.

Jos haluamme lisätä ajan tähän muotoon, voimme käyttää `"%d.%m.%Y %H.%M.%S"` muotoa, joka lisää tunnit, minuutit ja sekunnit. Lopputuloksena saamme merkkijonon "31.12.2021 00.00.00".

## Syvemmälle pinnan alle

Gleamissa päivämäärät käsitellään `Date`-moduulin avulla, joka perustuu Elixirin `DateTime`-moduuliin. Tämä moduuli tarjoaa laajan valikoiman erilaisia muotoilutapoja, joita voimme käyttää `format`-funktion kanssa. Joitakin esimerkkejä ovat:

- `"%Y-%m-%dT%H:%M:%S"` muotoilu, joka luo ISO 8601 yhteensopivan päivämäärän ja ajan muodon (esim. 2021-12-31T00:00:00)
- `"%a, %d %b %Y %H:%M:%S %z"` muotoilu, joka luo päivämäärän englanninkielisen päivämäärän ja ajan muodon (esim. Fri, 31 Dec 2021 00:00:00 +0000)
- `"%I:%M %p"` muotoilu, joka luo ajan muodon 12-tuntisessa muodossa (esim. 12:00 AM)

On myös hyvä huomata, että Gleam tukee myös Unix-timestampeja `Time.build` ja `Time.format` -funktioiden avulla.

## Katso myös

- [Gleam Date-moduuli dokumentaatio](https://gleam.run/lib/date.html)
- [Elixir DateTime-moduuli dokumentaatio](https://hexdocs.pm/elixir/DateTime.html)