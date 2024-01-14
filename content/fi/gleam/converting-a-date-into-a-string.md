---
title:                "Gleam: Päivämäärän muuntaminen merkkijonoksi"
programming_language: "Gleam"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit muuttaa päivämäärän merkkijonoksi? Tämä voi olla hyödyllistä esimerkiksi päivämäärän esittämiseksi käyttäjälle tai tiedoston nimen luomiseksi.

## Miten

```Gleam
import gleam/time.Date

let date = Date.new(2021, 9, 1)
let formatted_date = Date.format(date, "~D-~M-~yyyy")
```

Tämä koodi luo päivämäärä-olion ja muuttaa sen sitten toivotuksi merkkijonoksi käyttäen Date.format-funktiota. Tulostus on "1-9-2021".

## Syvällä

Päivämäärän muuttaminen merkkijonoksi Gleamin avulla perustuu olionmuotoilukieleen, joka on vahvasti muotoiltu Gleamissa. Tämä tarkoittaa, että muuttujan tietotyyppi ja sen muodostajat ovat tärkeitä.

Voit tutustua lisää Gleamin Date-moduulin dokumentaatioon saadaksesi täyden käsityksen eri muodostajien käyttämisestä ja päivämäärän muotoilun vaihtoehdoista.

## Katso myös

- Gleamin Date-moduulin dokumentaatio (englanniksi) https://gleam.run/modules/gleam/time.Date.html
- Date-olionmuotoilu Gleam-mallissa (englanniksi) https://gleam.run/manual/olionmuotoilu.html
- Gleam Community Slack (englanniksi) https://join.slack.com/t/gleam-lang/shared_invite/zt-m42jatxf-2ZVeSZb2r_P8wX84AfKuLg

Kiitos lukemisesta ja onnea Gleamin käytössä!