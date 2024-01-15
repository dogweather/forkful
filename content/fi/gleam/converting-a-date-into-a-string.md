---
title:                "Muunnetaan päivämäärä merkkijonoksi"
html_title:           "Gleam: Muunnetaan päivämäärä merkkijonoksi"
simple_title:         "Muunnetaan päivämäärä merkkijonoksi"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Miksi

Kenties olet törmännyt tarpeeseen muuntaa päivämäärä merkkijonoksi ohjelmassasi. Tämä artikkeli esittelee, miten Gleam-kielen avulla voit helposti ja tarkasti suorittaa tämän konversion.

# Miten

Aloitetaan luomalla päämoduuli, joka sisältää muutamia päivämääriä eri formaateissa.

```Gleam
// Haku pakkaukselle "gleam/datetime"
import gleam/datetime

// Päivämäärä muodossa "vuosi, kuukausi, päivä"
let date = datetime.Date.new(2021, 9, 30)

// Aika muodossa "tunti, minuutti, sekunti, millisekunti"
let time = datetime.Time.new(12, 30, 45, 500)

// Päivämäärä ja aika yhdistettynä
let datetime = datetime.DateTime.new(date, time)

// Tulostetaan päivämäärä ja aika merkkijonona
io.println(datetime |> datetime.to_string())
```

Tässä esimerkissä käytämme "gleam/datetime" -pakkauksessa olevaa Date ja Time moduuleja luodaksemme päivämäärän ja ajan. Sitten yhdistämme ne DateTime-moduulissa ja lopuksi muunnetaan merkkijonoksi käyttämällä to_string()-funktiota. Tulostetun merkkijonon pitäisi näyttää seuraavalta: "2021-09-30T12:30:45.500Z".

# Syväsukellus

Gleamissa päivämäärän ja ajan muuntaminen merkkijonoksi on mahdollista kielen ydinmoduulien avulla, mutta "gleam/datetime" -pakkauksen avulla voimme tehdä sen paljon helpommin ja tarkemmin. DateTime-moduulissa on myös muita hyödyllisiä funktioita, kuten muunnokset eri aikavyöhykkeiden välillä.

# Katso myös

- [Gleam-kielen viralliset sivut](https://gleam.run)
- [Gleam-dokumentaatio](https://gleam.run/documentation)
- [Gleam/datetime pakkauksen dokumentaatio](https://github.com/gleam-lang/datetime)