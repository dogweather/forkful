---
title:    "C#: Päivämäärän muuttaminen merkkijonoksi"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Miksi
Useimmat ohjelmat käsittelevät päivämääriä ja aikaa ainakin jossain vaiheessa. On tärkeää pystyä muuttamaan nämä arvot merkkijonoksi, jotta niitä voidaan käyttää esimerkiksi tulostuksessa tai tallentamisessa tietokantaan.

## Kuinka tehdä
Seuraava koodiesimerkki näyttää, kuinka muuttaa päivämäärä ja aika merkkijonoksi käyttäen C#-ohjelmointikieltä:

```C#
// Luo uusi DateTime-objekti annetuilla arvoilla
DateTime date = new DateTime(2021, 11, 5, 14, 30, 0);

// Käytä ToString-metodia muuttaaksesi päivämäärä ja aika merkkijonoksi
string dateString = date.ToString();

// Tulosta tulos konsoliin
Console.WriteLine(dateString);

// Output: 11/5/2021 2:30:00 PM
```

Toinen tapa muuttaa päivämäärä ja aika merkkijonoksi on käyttää ToString-metodin yhteydessä toista parametria, joka määrittää halutun muodon. Seuraava koodi muuttaa päivämäärän ja ajan englanninkieliseen muotoon:

```C#
string dateString = date.ToString("MM/dd/yyyy hh:mm:ss tt");

// Output: 11/05/2021 02:30:00 PM
```

On myös mahdollista määrittää oma muoto päivämäärälle ja ajalle käyttämällä erilaisia merkkejä ja merkkijonoja. Esimerkiksi seuraava koodi muuttaa päivämäärän ja ajan seuraavaan muotoon: "Perjantai 5. marraskuuta 2021 klo 14.30":

```C#
string dateString = date.ToString("dddd d. MMMM yyyy 'klo' HH.mm");

// Output: Perjantai 5. marraskuuta 2021 klo 14.30
```

Oman muodon määrittäminen voi olla hyödyllistä, jos haluat mukauttaa päivämäärän ja ajan ulkoasua tiettyyn tarkoitukseen.

## Syvällisempää tietoa
Päivämäärän ja ajan muuttaminen merkkijonoksi ei ole vaikeaa C#:ssa, mutta on tärkeää ymmärtää muutaman asian siitä, miten tämä toimii. Ensinnäkin, ToString-metodin käyttämä oletusmuoto voi vaihdella eri kielialueilla. Esimerkiksi englanninkielisessä ympäristössä päivämäärä ja aika muunnetaan yleensä kuukausi/päivä/vuosi -muotoon, mutta suomenkielisessä ympäristössä se voi olla päivä/kuukausi/vuosi. Tästä syystä on tärkeää määrittää haluttu muoto toisena parametrina ToString-metodille.

Toiseksi, päivämäärän ja ajan muuttaminen merkkijonoksi käyttäen ToString-metodia käyttää oletusarvoisesti kulttuuriasetuksia. Kulttuuriasetuksilla tarkoitetaan alueellisia asetuksia, kuten desimaalierottimen ja ajan muodon muotoilua. Jos haluat määrittää oman kulttuuriasetuksen, voit käyttää ToString-metodia kolmella parametrilla: haluttu muoto, kulttuuriasetukset ja IFormatProvider -rajapinnan toteuttava objekti.

## Katso myös
- [DateTime.ToString-metodi (C#-ohjelmointikieli)](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.tostring?view=net-5.0)
- [