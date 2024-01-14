---
title:    "Elm: Kirjoittaminen standardivirheeseen"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Miksi käyttää standardivirheen kirjoittamista

Standardivirheen kirjoittaminen on tärkeä osa ohjelmointia, koska se auttaa kehittäjiä tunnistamaan ja korjaamaan ohjelmakoodeissa olevia virheitä. Kun ohjelma luo virheen, se kirjoitetaan standardivirheeseen ja näin kehittäjä voi tarkastella virheen syytä ja korjata sen.

## Kuinka tehdä

```Elm
import Debug exposing (crash)
 
calculateArea : Float -> Float -> String
calculateArea length width =
   if length * width < 0 then 
      crash "Nelijyrmiön alue ei voi olla negatiivinen!"
   else
      "Nelijyrmiön alue on " ++ toString (length * width)
```

Tässä esimerkissä käytämme Elm Debug-moduulia luomaan virheilmoituksen, jos nelijyrmiön alue on negatiivinen. Tämä auttaa meitä tunnistamaan ja korjaamaan virheen ohjelmakoodissa. Tarkastelkaamme nyt esimerkkiä ja sen tulostetta.

```Elm
calculateArea 5 3
```

Tulos: Nelijyrmiön alue on 15

```Elm
calculateArea -2 4
```

Tulos: Tämä luo virheilmoituksen "Nelijyrmiön alue ei voi olla negatiivinen!", mikä auttaa meitä tunnistamaan ja korjaamaan virheen.

## Syvällisempi tieto standardivirheen kirjoittamisesta

Ohjelmakoodien lukeminen ja virheiden tunnistaminen voi olla haastavaa, mutta standardivirheen kirjoittaminen auttaa kehittäjiä selkeyttämään ohjelmakoodia ja tunnistamaan virheitä helpommin. On myös tärkeää muistaa, että standardivirheen kirjoittaminen ei ole ainoa tapa käsitellä virheitä ohjelmakoodissa, vaan on olemassa muita tapoja, kuten käyttäjäystävällisten virheilmoitusten luominen.

## Katso myös

- [Elm-sivuston virheenkäsittelydokumentaatio](https://guide.elm-lang.org/error_handling/)
- [Ohjeet virheenhallintaan Elmissä](https://elmprogramming.com/error-handling.html)
- [Debug-moduulin käyttö Elm-ohjelmoinnissa](https://package.elm-lang.org/packages/elm-lang/core/latest/Debug)