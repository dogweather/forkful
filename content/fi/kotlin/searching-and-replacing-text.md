---
title:    "Kotlin: Tekstin etsiminen ja korvaaminen."
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Miksi: Etsi ja Korvaa Teksti

Tervetuloa takaisin blogiin, joka on omistettu Kotlin-ohjelmointikieleen! Tänään puhumme etsinnästä ja korvaamisesta tekstissä. Jos olet koskaan halunnut nopeasti muuttaa useita kohtia tekstissäsi, tämä on juuri sinulle. Jatka lukemista, niin näytämme sinulle, kuinka voit käyttää Kotlinia etsiäksesi ja korvataksesi tekstiä helposti.

## Kuinka Tehdä: Esimerkkejä koodiblokeissa

Kotlinilla on jo valmiiksi sisäänrakennettu toiminto tekstihaulle ja korvaamiselle, mikä tekee työstä paljon helpompaa. Voit käyttää `replace`-funktiota ja antaa sille haluamasi etsittävän ja korvaavan tekstin. Esimerkiksi, jos haluat korvata kaikki sanan "kissa" sanoilla "koira", seuraava koodi voisi auttaa:

```Kotlin
val teksti = "Minulla on kaksi kissaa."
val korvattuTeksti = teksti.replace("kissa", "koira")

println(korvattuTeksti)
```

Tulostus: "Minulla on kaksi koiraa."

Voit myös käyttää regex-symboleja laajentamaan toiminnallisuutta, kuten korvaamalla kaikki numerot tekstissä tyhjällä merkillä:

```Kotlin
val teksti = "Minun puhelinnumeroni on 555-123-4567."
val korvattuTeksti = teksti.replace("\\d".toRegex(), "")

println(korvattuTeksti)
```

Tulostus: "Minun puhelinnumeroni on --."

## Syvemmälle: Etsinnän ja Korvaamisen Tarkemmat Tiedot

Nyt kun olet nähnyt joitain esimerkkejä, saatat ihmetellä, mitä muuta voit tehdä etsiessä ja korvatessa tekstiä Kotlinilla. Tässä muutama esimerkki lisäominaisuuksista, jotka saattavat tulla hyödyllisiksi:

- Voit käyttää `contains`-funktiota tarkistaaksesi, sisältääkö tekstisi tietyn merkkijonon.
- Voit lisätä toisen argumentin `replace`-funktioon, joka määrittää, kuinka monta kertaa haluat korvata tekstin. Esimerkiksi, vain ensimmäinen esiintyminen korvataan, kun käytät `replace("a", "b", 1)`.
- Voit myös käyttää `replaceFirst` tai `replaceLast` vaihtaaksesi vain ensimmäisen tai viimeisen esiintymän.
- Jos haluat vaihtaa vain osan tekstistä, voit käyttää regex-sanoja, kuten `\\b` merkkaamaan sanan rajan.

Kuten aina, suosittelemme tutustumista viralliseen dokumentaatioon lisätietoja varten.

## Katso Myös

- [Kotlinin tekstinkäsittelyopas](https://kotlinlang.org/docs/basic-types.html#strings)
- [Regex-pikakurssi](https://regexone.com/)

Kiitos kun luit tämän blogipostauksen! Toivottavasti tämä auttoi sinua ymmärtämään etsintää ja korvaamista Kotlinissa. Nähdään seuraavassa blogipostauksessamme, joka on omistettu yhtä tärkeälle aiheelle.