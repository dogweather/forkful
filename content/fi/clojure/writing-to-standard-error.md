---
title:    "Clojure: Kirjoittaminen standardivirheelle"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi

Kirjoittaminen standardivirheeseen on tärkeää virheenkorjaustyökalu. Kun koodi ei toimi oikein, näet tietoja virheestä ja sen sijainnista standardivirheessä.

## Miten

Käytä `clojure.core` -kirjastoa kirjoittaessasi standardivirheeseen. Voit tehdä tämän `(:require [clojure.core :refer :all])` ja kirjoita sitten `(*err* "Viestisi tänne")` tai `(*eprn* "Viestisi tänne" [+ 1 2])` jos haluat tulostaa lopputuloksen.

```Clojure
(:require [clojure.core :refer :all])

(*err* "Tämä on virhe. Tarkista koodisi.")

(*eprn* "Lopputulos: " [+ 1 2])
```

Tämä tulostaisi seuraavan standardivirheen:

```
Tämä on virhe. Tarkista koodisi.
Lopputulos: 3
```

## Syvempää tietoa

Standardivirheen kirjoittamisen lisäksi voit myös käyttää `*out*` -muuttujaa tulostamiseen standardilähtöön ja `*in*` -muuttujaa lukemiseen standardisyötteestä. Nämä muuttujat ovat osa `clojure.core` -kirjastoa ja voivat auttaa sinua kommunikoimaan koodisi kanssa.

## Katso myös

- [The Clojure Cheatsheet](https://clojure.org/api/cheatsheet)
- [Clojure Dokumentaatio](https://clojuredocs.org/)
- [Lyhyt opas Clojureen](https://clojure.org/guides/getting_started)