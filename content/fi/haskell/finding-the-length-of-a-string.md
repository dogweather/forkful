---
title:                "Merkkijonon pituuden löytäminen"
html_title:           "Haskell: Merkkijonon pituuden löytäminen"
simple_title:         "Merkkijonon pituuden löytäminen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Oletko koskaan tarvinnut tietää merkkijonon pituuden ohjelmointiprojektissasi? Tai ehkä haluat vain tutustua uuteen ohjelmointikielen ominaisuuteen? Tässä artikkelissa opit, miten voit käyttää Haskell-kielellä valmiina olevaa toimintoa merkkijonon pituuden löytämiseen.

## Miten

Haskellissa merkkijonon pituuden löytämiseen on helppo ja nopea tapa. Voit käyttää standardikirjaston `length` funktiota antamalla sille merkkijonon parametrina. Katsotaan esimerkkiä:

```Haskell
length "Tämä on esimerkkimerkkijono"
```
Tämän koodin suorittaminen johtaa tulokseen `28`, eli merkkijonon pituus on 28 merkkiä. Kuten näet, `length` funktio palauttaa kokonaislukuarvon, joten voit tallentaa sen muuttujaan tai käyttää sitä suoraan muissa operaatioissa.

Vaihtoehtoisesti voit myös käyttää `Data.Text` moduulin `length` funktiota merkkijonojen käsittelyyn. Tämä funktio on suorituskykyisempi ja tehokkaampi kuin standardikirjaston vastaava. Katsotaan esimerkkiä:

```Haskell
import Data.Text (length)

length "Tämä on esimerkkimerkkijono"
```
Tuloksena saadaan jälleen `28`, mutta tämä funktio toimii nopeammin ja tehokkaammin toistuville operaatioille.

## Syvemmälle

Miten sitten `length` funktio toimii taustalla? Siinä hyödynnetään yhtä Haskellin perustietorakenteista, lista-tyyppiä. Listassa on alkioiden lisäksi myös pituustieto, joten `length` funktio käyttää tätä tietoa vain palauttaakseen alkioihin perustuvan pituuden.

On myös hyvä huomata, että `length` funktio toimii vain merkkijonon ensimmäisen tason pituuden laskemisessa. Jos haluat laskea sisäkkäisten listojen pituudet, sinun tulee käyttää rekursiota tai muita sopivia funktioita.

## Katso Myös

- [Haskellin dokumentaatio listojen pituudesta](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-List.html#v:length)
- [Data.Text moduulin dokumentaatio](https://hackage.haskell.org/package/text-1.2.4.1/docs/Data-Text.html)