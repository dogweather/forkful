---
title:                "Nykyisen päivämäärän saaminen"
html_title:           "Gleam: Nykyisen päivämäärän saaminen"
simple_title:         "Nykyisen päivämäärän saaminen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Saa päivämäärästä on prosessi, jossa tietokonejärjestelmä hakee ja näyttää nykyisen päivämäärän. Ohjelmoijat tekevät tämän saadakseen tarkkaa tietoa ajasta, jota he voivat käyttää esimerkiksi tapahtumien seuraamiseen ja aikaleimojen tallentamiseen.

## Miten:
### Gleam:ssa
```Gleam
import gleam/time

fn get_current_date() {
  let today = time.now() // hakee nykyisen päivämäärän
  time.format(today, "%d.%m.%Y") // muotoilee sen suomalaiseen muotoon päivä.kuukausi.vuosi
}

show(get_current_date())
```

[Try it online](https://gleam.run/?code=import%20gleam%2Ftime%0A%0Afn%20get_current_date()%20%7B%0A%20%20let%20today%20%3D%20time.now()%0A%20%20time.format(today%2C%20%22%25d.%25m.%25Y%22)%0A%7D%0A%0Ashow(get_current_date()))

### Muilla kielillä
```JavaScript
const today = new Date()
const day = today.getDate()
const month = today.getMonth() + 1
const year = today.getFullYear()

console.log(day + "." + month + "." + year)
```

```Python
import datetime

today = datetime.date.today()
print(today.strftime("%d.%m.%Y"))
```

## Syväsukellus:
Päivämäärän saaminen on tärkeä osa monien ohjelmien toimintaa, esimerkiksi kalentereissa ja tapahtumaseurannassa. Nykyään on helppoa ja nopeaa hakea nykyinen päivämäärä ja muotoilla se haluttuun muotoon, kuten suomalaiseen päivä-kuukausi-vuosi -muotoon.

On myös muita tapoja saada nykyinen päivämäärä, kuten käyttämällä komentoriviä tai käyttöjärjestelmän kehitystyökaluja. Kuitenkin Gleam:n tarjoama `time` moduuli tekee tästä tehtävästä erittäin yksinkertaista ja selkeää.

## Katso myös:
- [Gleam:n `time` moduuli](https://gleam.run/libraries/time/)
- [Stack Overflow:n vastaukset nykyisen päivämäärän saamiseen eri kielillä](https://stackoverflow.com/questions/2949957/get-current-time-in-python)