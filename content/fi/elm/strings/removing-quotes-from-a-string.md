---
date: 2024-01-26 03:38:53.995088-07:00
description: "Miten: Elmiss\xE4 voit k\xE4ytt\xE4\xE4 `String`-funktioita merkkijonojen\
  \ manipulointiin, kuten lainausmerkkien poistamiseen. T\xE4ss\xE4 on suoraviivainen\
  \ tapa tehd\xE4 se."
lastmod: '2024-03-13T22:44:56.476353-06:00'
model: gpt-4-0125-preview
summary: "Elmiss\xE4 voit k\xE4ytt\xE4\xE4 `String`-funktioita merkkijonojen manipulointiin,\
  \ kuten lainausmerkkien poistamiseen."
title: Merkkijonosta lainausmerkkien poistaminen
weight: 9
---

## Miten:
Elmissä voit käyttää `String`-funktioita merkkijonojen manipulointiin, kuten lainausmerkkien poistamiseen. Tässä on suoraviivainen tapa tehdä se:

```Elm
removeQuotes : String -> String
removeQuotes str =
    String.trim (String.filter (\char -> char /= '\"' && char /= '\'') str)

main =
    String.removeQuotes "\"Tämä on 'lainattu' merkkijono!\""
    -- Tuloste: Tämä on lainattu merkkijono!
```

Muista vain: tämä pieni pätkä poistaa kaikki lainausmerkit merkkijonostasi, joten käytä sitä viisaasti!

## Syventävä sukellus
Aikaisemmin merkkijonojen käsittely oli hieman enemmän käsin tehtävää, johon sisältyi paljon manuaalista jäsentämistä. Nykyään kielet, kuten Elm, tekevät siitä yksinkertaisempaa sisäänrakennettujen funktioiden avulla. Funktio `String.filter` on monipuolinen työkalu arsenaliisi, kun tarvitset nirsoilla jokaisesta merkistä, mikä sisältää mutta ei rajoitu lainausmerkkien poistamiseen.

Vaihtoehtoisesti saatat turvautua säännöllisiin lausekkeisiin, jos Elm tukisi niitä siirrettävästi, mitä se ei oletusarvoisesti tee. Mutta hei, Elmin keskittyminen yksinkertaisuuteen ja turvallisuuteen tarkoittaa, että `String.filter` -lähestymistapamme on selkeä, turvallinen ja helppo ylläpitää.

Elmin funktionaalinen lähestymistapa kannustaa puhtaiden funktioiden käyttöön ilman sivuvaikutuksia, ja `removeQuotes` on loistava esimerkki. Se ottaa merkkijonon ja palauttaa uuden, jättäen alkuperäisen vahingoittumattomaksi. Se on Elmin muuttumattomien tietorakenteiden ansiota, edistäen ennustettavuutta ja helpottaen debuggauskärsimystäsi.

## Katso myös
Lisätietoja ja liittyviä merkkijonojen käsittelyseikkailuja varten tutustu Elmin `String`-moduulin dokumentaatioon:

- [Elm String Docs](https://package.elm-lang.org/packages/elm/core/latest/String)

Ja jos koskaan olet pulassa siitä, mitä Elm tukee merkkijonokäsittelyn tai minkä tahansa kielen ominaisuuden suhteen:

- [Elm Language Guide](https://guide.elm-lang.org/)
