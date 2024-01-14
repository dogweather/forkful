---
title:    "Bash: Päivämäärän haku"
keywords: ["Bash"]
---

{{< edit_this_page >}}

# Miksi: Miksi on tärkeää selvittää nykyinen päivämäärä?

Monissa Bash-skripteissä on tarpeen saada nykyinen päivämäärä ja aika, jotta skripti voi suorittaa halutun toiminnon oikeaan aikaan. Esimerkiksi tiedostojen varmuuskopiointi- tai loki-skriptiä voi haluta ajaa joka päivä ja tallentaa varmuuskopio tai loki tiedostonimeen, johon sisältyy nykyinen päivämäärä. Tämä tekee tiedostojen järjestelyn helpommaksi ja auttaa selvittämään, milloin mikäkin toiminto on suoritettu.

## Kuinka: Kuinka saada nykyinen päivämäärä Bash-skriptissä?

Bashilla on useita erilaisia komentoja, joilla voit saada päivämäärä ja aika muodossa, joka sopii skriptisi tarpeisiin. Yksi yksinkertainen tapa on käyttää `date` komentoa ja muotoilla päivämäärä haluamallasi tavalla.

Esimerkiksi voit käyttää tätä komentoa saadaksesi nykyisen päivämäärän muodossa "pv.kk.vvvv":

```Bash
date +"%d.%m.%Y"
```

Tämän komennon tuloste voisi näyttää esimerkiksi tältä:

```Bash
24.03.2021
```

Voit myös käyttää muita komennon vaihtoehtoja muotoillessasi päivämäärää, kuten lisätä kellonajan tai aikavyöhykkeen. Kannattaa tutkia `date` komennon manuaalisivua lisätietojen saamiseksi.

## Syvempi sukellus: Päivämäärän hankkiminen Bash-skriptissä

Bash on Linux-komentotulkin mukana tuleva ohjelmointikieli, joka tekee siitä helpon ja monipuolisen tavan hankkia päivämäärä ja aika skriptin suorituksen aikana.

Voit käyttää myös `date` komennon lisäksi muita Bashin sisäänrakennettuja muuttujia, kuten `$DATE`, joka sisältää nykyisen päivämäärän, tai `$TIME`, joka sisältää nykyisen ajan. Näiden muuttujien sisältöä voi muokata tai käyttää osana tiedostonimeä tai muuta toimintoa.

Lisäksi voit suorittaa Bash-skriptit automaattisesti tiettyinä päivinä tai aikoina käyttämällä `crontab` -ohjelmaa. `crontab` mahdollistaa toistuvien tehtävien suorittamisen määritetyssä aikataulussa, joten voit esimerkiksi luoda skriptin, joka suorittaa varmuuskopioinnin joka ilta.

# Katso myös:

- `date` manuaalisivu
- Bash scripting tutorial (englanniksi)
- crontab Wikipedia-artikkeli (englanniksi)