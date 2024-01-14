---
title:                "Gleam: Päivämäärän haku"
programming_language: "Gleam"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi hankkia nykyinen päivämäärä?

Tulevien tapahtumien suunnittelu, tiedon tallentaminen ja aikaleimojen luominen ovat vain muutamia syitä, miksi joku haluaisi hankkia nykyisen päivämäärän ohjelmoinnissa. Onneksi Glean avulla tämä on helppoa.

## Kuinka tehdä se:

```Gleam
import Gleam.DateTime
import Gleam.Pipe

let current_date =
  DateTime.local_now()
  |> Date.to_string()
```

Tämä yksinkertainen koodinpätkä käyttää DateTime-moduulia hankkimaan nykyisen päivämäärän ja sitten Date-moduulia muuntamaan sen merkkijonoksi. Voit myös muuttaa merkkijonon muotoilua käyttämällä Date.to_string_opts-funktiota ja antamalla haluamasi muodon. Esimerkiksi "YYYY-mm-dd".

## Syvällinen sukellus:

Jos haluat syvemmän ymmärryksen siitä, miten DateTime ja Date-moduulit toimivat, voit tarkastella niiden lähdekoodia. DateTime-moduuli käyttää Erlangin :os.date -toimintoa ja Date-moduuli käyttää :calendar.now_to_datetime-funktiota.

## Katso myös:

- Glean DateTime-dokumentaatio: https://gleam.run/modules/gleam_datetime
- Gleam Date-dokumentaatio: https://gleam.run/modules/gleam_date