---
title:                "Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä"
html_title:           "C#: Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä"
simple_title:         "Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

Usein tarvitsemme laskemaan päivämäärän menneisyydessä tai tulevaisuudessa toiminnassamme. Ehkä sinulla on projekti, joka vaatii tietyn päivämäärän tai haluat vain tietää tulevaisuuden päivämäärän johonkin tiettyyn tapahtumaan.

## Kuinka

Laskeminen päivämäärää menneisyydessä on helppoa C# -ohjelmointikielellä. Seuraavassa on esimerkki, joka näyttää kuinka voit laskea päivämäärän tietyn määrän päiviä menneisyyteen:

```C#
DateTime nykyinenPäivä = DateTime.Today; //Hakee nykyisen päivämäärän
int päivätMenneisyydessä = 30; //Määrittää, kuinka monta päivää haluat mennä taaksepäin

DateTime tulevaPäivä = nykyinenPäivä.AddDays(-päivätMenneisyydessä); //Laskee päivämäärän 30 päivää taaksepäin
Console.WriteLine(tulevaPäivä); //Tulostaa lasketun päivämäärän konsoliin
```

Tämä koodi käyttää DateTime-luokan AddDays-metodia, joka lisää tai vähentää päiviä valittuun päivämäärään. Huomaa, että käytämme negatiivista päivien lukumäärää, jos haluamme laskea päivämäärää menneisyyteen.

Voit myös laskea tulevan päivämäärän lisäämällä päiviä nykyiseen päivämäärään. Seuraavassa esimerkissä lisätään 30 päivää nykyiseen päivämäärään:

```C#
DateTime nykyinenPäivä = DateTime.Today; //Hakee nykyisen päivämäärän
int päivätTulevaisuudessa = 30; //Määrittää, kuinka monta päivää haluat lisätä

DateTime tulevaPäivä = nykyinenPäivä.AddDays(päivätTulevaisuudessa); //Laskee päivämäärän 30 päivää eteenpäin
Console.WriteLine(tulevaPäivä); //Tulostaa lasketun päivämäärän konsoliin
```

## Syvempi sukellus

Päivämäärän laskeminen menneisyydessä tai tulevaisuudessa on mahdollista myös käyttämällä TimeSpan-luokkaa. TimeSpan-luokka edustaa ajanjaksoa esimerkiksi päivien, tuntien tai minuuttien muodossa. Voimme käyttää tätä luokkaa laskemaan päivämäärä tietyn ajanjakson kuluttua.

Seuraavassa esimerkissä lisätään 30 päivää nykyiseen päivämäärään käyttäen TimeSpania:

```C#
DateTime nykyinenPäivä = DateTime.Today; //Hakee nykyisen päivämäärän
int päivätTulevaisuudessa = 30; //Määrittää, kuinka monta päivää haluat lisätä

TimeSpan ajanjakso = new TimeSpan(päivätTulevaisuudessa, 0, 0, 0); //Luodaan TimeSpan jossa 30 päivää
DateTime tulevaPäivä = nykyinenPäivä + ajanjakso; //Lisätään TimeSpan nykyiseen päivämäärään
Console.WriteLine(tulevaPäivä); //Tulostaa lasketun päivämäärän konsoliin
```

Huomaathan, että TimeSpanin ensimmäinen parametri on päivien määrä. Voit myös käyttää muita parametreja, kuten tunteja, minuutteja ja sekunteja, lasketaksesi tarkempia päivämääri