---
title:                "Swift: Päivämäärän hankkiminen"
programming_language: "Swift"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä miksi haluaisi saada näytön päivämäärän Swift-ohjelmoinnissa. Tämä voi olla hyödyllistä esimerkiksi tapahtumien aikaleimojen tallentamiseen tai suorituskyvyn mittaamiseen eri päivien välillä.

## Kuinka

Käyttämällä Swiftin ```Date()```-funktiota, voit helposti saada nykyisen päivämäärän ja ajan. Tämä palauttaa päivämäärän ja ajan nykyisestä aikavyöhykkeestäsi.

```
let currentDate = Date()
print(currentDate)
```

Tämä koodi tulostaa nykyisen päivämäärän ja ajan seuraavassa muodossa:

```
2019-09-10 18:28:30 +0000
```

Voit myös muuttaa päivämäärän muotoa käyttämällä ```DateFormatter```-luokkaa. Tässä esimerkissä päivämäärä muutetaan "dd/MM/yyyy" muotoon:

```
let formatter = DateFormatter()
formatter.dateFormat = "dd/MM/yyyy"

let currentDate = Date()
print(formatter.string(from: currentDate))
```

Tämä tulostaa nykyisen päivämäärän muodossa:

```
10/09/2019
```

## Syvenny

Päivämäärän saaminen voi joskus olla monimutkaisempaa, erityisesti jos tarvitset tarkempaa tietoa päivämäärästä, kuten vuoden, kuukauden tai päivän numeron. Tähän voit käyttää ```Calendar```-luokkaa, joka antaa sinulle lisätoiminnallisuutta päivämäärään liittyen.

```
let calendar = Calendar.current
let year = calendar.component(.year, from: Date())
let month = calendar.component(.month, from: Date())
let day = calendar.component(.day, from: Date())

print("Nykyinen vuosi: \(year)")
print("Nykyinen kuukausi: \(month)")
print("Nykyinen päivä: \(day)")
```

Tämä koodi palauttaa seuraavan tulosteen:

```
Nykyinen vuosi: 2019
Nykyinen kuukausi: 9
Nykyinen päivä: 10
```

## Katso myös

- Apple Developer Documentation: [Date Class](https://developer.apple.com/documentation/foundation/date)
- Apple Developer Documentation: [DateFormatter Class](https://developer.apple.com/documentation/foundation/dateformatter)
- Apple Developer Documentation: [Calendar Class](https://developer.apple.com/documentation/foundation/calendar)