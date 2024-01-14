---
title:    "Bash: Päivämäärän hankkiminen"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi
Tervetuloa lukemaan uutta blogipostausta Bash-ohjelmoinnista! Tässä artikkelissa käsitellään kuinka saat nykyisen päivämäärän Bash-skriptin avulla ja miksi tämä taito on hyödyllinen.

## Kuinka
Bash-ohjelmointi on tärkeä osa monia IT-alan tehtäviä, ja usein haluamme käyttää skriptejä tehdäksemme päivittäisiä tehtäviämme helpommiksi. Yksi näistä tehtävistä voi olla nykyisen päivämäärän hakeminen. Voit tehdä tämän näppärästi Bash-skriptien avulla.

Aloitetaan esittelyllä eräästä Bash-ohjelmoinnin perustyökalusta, nimeltään `date`. Tämä komento näyttää nykyisen päivämäärän ja ajan seuraavassa muodossa:

```Bash
Wed Sep 1 16:53:22 UTC 2021
```

Mutta mitä jos haluat muuttaa päivämäärän esitysmuotoa tai tulostaa vain tietyn osan päivämäärästä? Tässä muutamia esimerkkejä, kuinka se voidaan tehdä käyttämällä `date` komentoa:

Haluatko nähdä vain nykyisen kuukauden numeron?

```Bash
date +%m
```

Tämä komento tulostaa pelkästään numeron, joka vastaa nykyistä kuukautta. Esimerkiksi tällä hetkellä se näyttäisi "09".

Entä jos tarvitset vuoden kahdella viimeisellä numerolla?

```Bash
date +%y
```

Tämä näyttää nykyisen vuoden kahdella viimeisellä numerolla, esimerkiksi "21".

Voit myös tulostaa nykyisen päivämäärän tiettynä muodossa. Tässä muutamia esimerkkejä:

```Bash
date +%d/%m/%Y
```

Tämä tulostaa päivämäärän muodossa päivä/kuukausi/vuosi. Esimerkiksi "01/09/2021".

```Bash
date "+Today is %A, %B %d, %Y"
```

Tämä tulostaa lauseen "Tänään on viikonpäivä, kuukausi päivä, vuosi". Esimerkiksi "Today is Wednesday, September 1, 2021".

Voit myös yhdistää näitä erilaisia formaatteja saadaksesi juuri haluamasi tuloksen. Kokeile rohkeasti erilaisia vaihtoehtoja ja löydä paras ratkaisu juuri sinun tarpeisiisi.

## Syvempi sukellus
Nyt kun olet saanut pienen maistiaisen kuinka `date` toimii Bash-skripteissä, voit halutessasi oppia lisää tästä komennosta. Voit aloittaa lukemalla sen manuaalisivun komentolla `man date` tai käyttämällä verkkoresursseja kuten tämän [linkin](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html).

Yksi hyödyllinen lisäominaisuus `date` komennossa on ajankohdan muutos. Voit esimerkiksi käyttää komentoa `date -d'30 days ago'`, joka näyttää päivämäärän, joka on 30 päivää aikaisemmin nykyisestä päivästä. Tutustu tarkemmin eri vaihtoehtoihin ja löydät varmasti auttavan ratkaisun juuri sinulle.

## Katso myös 
Halutessasi opiskella lisää Bash-ohjelmoinnista tai muista vastaavista aiheista, suosittelemme seuraavia resursseja:

- [Bash-suomen