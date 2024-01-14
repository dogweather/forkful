---
title:    "Gleam: Tietokoneohjelmointi: Kirjoittaminen standardivirheeseen"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi kirjoittaa standardi virheen syöttöön
Kirjoittaminen standardi virheen syöttöön voi olla hyödyllistä, kun haluat tallentaa virheitä ja ilmoituksia ohjelman suorituksen aikana. Tämä voi auttaa sinua tunnistamaan ja korjaamaan mahdollisia ongelmia ja virheitä koodissasi.

## Kuinka tehdä se
```Gleam
fn write_to_stderr(message) {
    io.stderr.write(message)
}

let error_message = "Tämä on virheilmoitus."
write_to_stderr(error_message)
```

Tämä koodi esimerkki näyttää, miten voit määritellä toiminnon kirjoittamaan viestin standardi virheen syöttöön ja kutsua sitä haluamallasi viestillä. Alla oleva tulos näyttää, miten viesti tulostuu standardi virheen syöttöön:

```
Tämä on virheilmoitus.
```

Tämä toiminnallisuus voi olla erityisen hyödyllinen silloin, kun ajetaan koodia komentokehotteessa ja halutaan tallentaa mahdollisia virheilmoituksia talteen myöhempää tarkastelua varten.

## Syvempi sukellus
Kun kirjoitat standardi virheen syöttöön Gleamissa, huomioi että viestin tulostaminen standardi virheen syöttöön ei pysäytä ohjelman suoritusta. Jos haluat pysäyttää suorituksen, voit käyttää `error!` toimintoa, joka tulostaa viestin ja pysäyttää ohjelman suorituksen.

## Katso myös
- Gleam virallinen dokumentaatio: https://gleam.run/
- Gleam virallinen GitHub sivusto: https://github.com/gleam-lang/gleam
- Gleam virallisilla keskustelufoorumeilla: https://gleam.run/chat