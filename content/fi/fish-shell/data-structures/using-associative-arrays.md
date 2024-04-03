---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:11:11.133810-07:00
description: "Assosiatiiviset taulukot eli hajautustaulut mahdollistavat tietojen\
  \ tallentamisen avain-arvo -pareina, mik\xE4 tekee tiedon j\xE4rjest\xE4misest\xE4\
  \ ja hakemisesta\u2026"
lastmod: '2024-03-13T22:44:56.985021-06:00'
model: gpt-4-0125-preview
summary: "Assosiatiiviset taulukot eli hajautustaulut mahdollistavat tietojen tallentamisen\
  \ avain-arvo -pareina, mik\xE4 tekee tiedon j\xE4rjest\xE4misest\xE4 ja hakemisesta\
  \ avaimen perusteella helpompaa."
title: "Assosiatiivisten taulukoiden k\xE4ytt\xF6"
weight: 15
---

## Miten:
Fish ei natiivisti tue assosiatiivisia taulukoita kuten Bash 4+, mutta voit saavuttaa samankaltaista toiminnallisuutta käyttämällä yhdistelmää listoja ja merkkijonomanipulaatiota. Näin voit jäljitellä niitä:

Ensimmäisenä, asetetaan "assosiatiiviset taulukon" elementit erikseen:

```Fish Shell
set food_color_apple "punainen"
set food_color_banana "keltainen"
```

Elementtiä käytetään viittaamalla siihen suoraan:

```Fish Shell
echo $food_color_apple
# Tuloste: punainen
```

Jos tarvitset iteroida niiden yli, käytä for-silmukkaa ottaen huomioon nimeämiskonvention:

```Fish Shell
for food in apple banana
    echo $food_color_$food
end
# Tuloste:
# punainen
# keltainen
```

Niille, jotka kaipaavat Bashin `${!array[@]}` toiminnallisuutta saadakseen kaikki avaimet, voit tallentaa avaimet erilliseen listaan:

```Fish Shell
set food_keys apple banana

for key in $food_keys
    echo $key 'on' $food_color_$key
end
# Tuloste:
# apple on punainen
# banana on keltainen
```

## Syväsukellus
Todelliset assosiatiiviset taulukot, kuten muissa skriptikielissä, eivät vielä ole osa Fishin lähestymistapaa. Näytetty kiertotapa hyödyntää Fishin merkkijonomanipulaation ja listan mahdollisuuksia luomaan pseudo-assosiatiivisen taulukkorakenteen. Vaikka se toimii, se ei ole yhtä siisti tai virheetön kuin sisäänrakennettu assosiatiivisten taulukoiden tuki olisi. Muut kuoret, kuten Bash ja Zsh, tarjoavat sisäänrakennetun assosiatiivisen taulukon toiminnallisuuden, joka johtaa suoraviivaisempaan, selkeämpään koodiin. Kuitenkin Fishin suunnittelufilosofia pyrkii yksinkertaisuuteen ja käyttäjäystävällisyyteen, mahdollisesti tällaisten ominaisuuksien kustannuksella. Kiertotapa tyydyttää suurimman osan tarpeista, mutta pidä silmällä Fish Shellin kehitystä—sen kehittäjät aktiivisesti parantavat ja lisäävät ominaisuuksia yhteisön palautteen perusteella.
