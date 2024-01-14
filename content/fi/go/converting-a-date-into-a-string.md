---
title:    "Go: Muuntaa päivämäärä merkkijonoksi"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Go-ohjelmointikielen yksi kätevä ominaisuus on kyky muuntaa päivämäärä tekstiksi. Tämä voi olla hyödyllistä esimerkiksi käyttäjien nähdessä päivämäärän selkeässä muodossa tai tiedot tallennettaessa tietokantaan.

## Miten

Ei ole yksinkertaista tapaa muuntaa Go-ohjelmassa päivämäärää merkkijonoksi, vaan se vaatii muutaman vaiheen suorittamista. Ensimmäinen askel on määritellä päivämäärämuuttuja ja sitten käyttää aikaisempaa pakettia Go-ohjelmassa, joka sisältää ylimääräisiä työkaluja ja toimintoja. Käytämme time-pakettia tähän esimerkkiin.

```
Go func main() {
    // Määritä päivämäärämuuttuja
    date := time.Date(2021, time.June, 4, 12, 30, 0, 0, time.UTC)

    // Käytä format-metodia muuntaaksesi päivämäärä merkkijonoksi
    fmt.Println(date.Format("Mon January 2, 2006"))
}
```

Tämä yksinkertainen esimerkki tulostaa "Fri June 4, 2021". Huomaa, että käytämme tiettyä formaattia "Mon January 2, 2006", joka vastaa tarkalleen päivämäärän näyttämistä. Voit vaihtaa tämän haluamasi formaattia vastaavaksi, kun valitset haluamasi tiedot.

## Syvällinen sukellus

Aikaisempi esimerkki on melko yksinkertainen, mutta Go tarjoaa myös muita käyttökelpoisia toimintoja päivämäärän muuntamiseen. Voit esimerkiksi muuttaa päivämäärän UNIX-aikaleimaksi käyttämällä Unix-metodia ja annettu päivämäärä muunnetaan desimaaliluvuksi, joka edustaa sekunteina kuluneita aikoja UNIX-ajan alusta.

Voit myös muuttaa päivämäärän eri aikavyöhykkeelle time-paketissa olevien toimintojen avulla. Tämä on erityisen hyödyllistä, jos työskentelet kansainvälisten käyttäjien kanssa tai haluat muuntaa ajan paikalliseen aikavyöhykkeeseen ennen tallennusta.

## Katso myös

- [Go:n aikapaketin dokumentaatio](https://pkg.go.dev/time)
- [Unix-aikaleima Google Developer](https://developers.google.com/analytics/devguides/reporting/realtime/v3/reference/data/realtime/get#examples)
- [Aikavyöhykkeen muuttaminen Go-ohjelmassa TutorialEdge] (https://tutorialedge.net/golang/manipulating-dates-go/)