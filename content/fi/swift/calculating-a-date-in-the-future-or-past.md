---
title:    "Swift: Tulevaisuuden tai menneen päivämäärän laskeminen"
keywords: ["Swift"]
---

{{< edit_this_page >}}

# Miksi: Laskeminen tulevaan tai menneeseen päivämäärään on hyödyllistä

Laskeminen tulevaan tai menneeseen päivämäärään voi olla hyödyllistä esimerkiksi silloin, kun haluat automaattisesti päivittävän aikataulun tai tehdä aikavertailuja. Se voi myös auttaa sinua muistamaan tärkeitä tapahtumia tulevaisuudessa tai tarkistamaan menneitä tapahtumia.

# Kuinka: Koodiesimerkkejä laskemisesta tulevaan tai menneeseen päivämäärään Swift-ohjelmointikielellä

Seuraavassa esimerkissä lasketaan tulevaa päivämäärää 30 päivää nykyisestä päivästä käyttämällä DateComponents-luokkaa ja Calendar-luokkaa:

```Swift
let currentDate = Date()
var dateComponents = DateComponents()
dateComponents.day = 30
let futureDate = Calendar.current.date(byAdding: dateComponents, to: currentDate)
print(futureDate)
```

Tämä tulostaisi "Optional(2020-03-23 08:03:01 +0000)", eli päivämäärä olisi 30 päivää nykyisestä päivästä.

Voit myös laskea menneen päivämäärän samaa logiikkaa käyttäen, mutta käyttämällä negatiivista arvoa päivien määrässä:

```Swift
let currentDate = Date()
var dateComponents = DateComponents()
dateComponents.day = -30
let pastDate = Calendar.current.date(byAdding: dateComponents, to: currentDate)
print(pastDate)
```

Tämä tulostaisi "Optional(2020-01-22 08:03:01 +0000)", eli päivämäärä olisi 30 päivää sitten nykyisestä päivästä.

# Syvempi sukellus: Lisää tietoa päivämäärän laskemisesta tulevaisuuteen tai menneisyyteen

DateComponents-luokka mahdollistaa laskemisen muilla aikayksiköillä, kuten vuosilla, kuukausilla, tunneilla ja minuuteilla. Voit myös käyttää manya muita vaihtoehtoja, kuten päivämäärän vertailua ja päivämäärämuodostusta DateInterval-luokan avulla.

On kuitenkin tärkeää huomata, että päivämäärämuutosten laskeminen voi olla monimutkaista ottaen huomioon aikavyöhykkeet ja kesäaika. On tärkeää olla varovainen näitä asioita käsitellessä.

# Katso myös

- Swiftin päivämäärä- ja aikatyökalut: https://developer.apple.com/documentation/foundation/date
- Opas aikavyöhykkeisiin Swiftissä: https://www.swiftbysundell.com/basics/timezones-in-swift/
- Esimerkkejä päivämäärän laskemisesta Swiftillä: https://nemecek.be/blog/149/p/date-components-add-subtract-dates-reference-guide