---
title:                "Alimerkkijonon erottaminen"
html_title:           "Swift: Alimerkkijonon erottaminen"
simple_title:         "Alimerkkijonon erottaminen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi 

Substringien erottaminen on yleinen tarve ohjelmoinnissa, erityisesti kun käsitellään tekstiä tai merkkijonoja. Se mahdollistaa tietyn osan tekstistä poimimisen ja sen käsittelemisen erillisenä kokonaisuutena.

## Kuinka

```Swift
let string = "Tämä on esimerkkiteksti"
let substring = string.suffix(10)
print(substring)
```

Tämä koodinpätkä ottaa alkuperäisestä merkkijonosta viimeiset 10 merkkiä ja tallentaa ne uuteen muuttujaan nimeltä "substring". Tämän jälkeen tulostetaan uuden muuttujan sisältö, jolloin tulosteena näkyy "va marteksti".

## Syvällisempi sukellus

Substringien erottamisessa on useita eri tapoja riippuen siitä, mitä halutaan saavuttaa. `prefix(_:)`- ja `suffix(_:)`-funktiot mahdollistavat halutun osan merkkijonosta poimimisen sen alusta tai lopusta. `dropFirst(_:)`- ja `dropLast(_:)`-funktiot taas poistavat halutun määrän merkkejä merkkijonon alusta tai lopusta.

Lisäksi voidaan käyttää `range(of:)`-funktiota, joka palauttaa halutun osan merkkijonosta, jos se löytyy siitä. Tämä on hyödyllistä esimerkiksi silloin, kun halutaan etsiä tiettyä sanaa tai lausetta merkkijonosta.

## Katso myös
- [Swiftin virallinen dokumentaatio substringeista](https://developer.apple.com/documentation/swift/substring)
- [Substringien erottaminen video-ohje](https://www.youtube.com/watch?v=tzlEbYwFi3E)
- [Swift-kurssi: Merkkijonot ja substringit](https://www.udemy.com/course/swift-programming-for-beginners/#/?utm_source=adwords-brand&utm_medium=udemyads&utm_campaign=CPP.VISIBLE.BRAND.AW&utm_content=deal4584&utm_term=_._ag_114114381124_._ad_461091568045_._kw__._de_c_._dm__._pl__._ti_dsa-1007766171312_._li_1001798_._pd__._&matchtype=b&gclid=Cj0KCQjw3duCBhCAARIsAJeFyPXJSyLGiQie3_6I1Ml_Ck-Byw2FI1r983x_89chQyiIAOV97vLRhYYaAk8HEALw_wcB)