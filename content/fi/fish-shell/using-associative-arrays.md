---
title:                "Assosiatiivisten taulukoiden käyttö"
date:                  2024-01-30T19:11:11.133810-07:00
model:                 gpt-4-0125-preview
simple_title:         "Assosiatiivisten taulukoiden käyttö"

category:             "Fish Shell"
tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä ja Miksi?

Assosiatiiviset taulukot eli hajautustaulut mahdollistavat tietojen tallentamisen avain-arvo -pareina, mikä tekee tiedon järjestämisestä ja hakemisesta avaimen perusteella helpompaa. Ne ovat käteviä, kun tarvitset rakenteellisemman tavan käsitellä tietoja kuin pelkät listat, erityisesti konfiguraatioissa ja kun käsitellään joukkoa attribuutteja.

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
