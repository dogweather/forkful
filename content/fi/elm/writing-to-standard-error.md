---
title:    "Elm: Tietokoneohjelmoinnissa standardivirheen kirjoittaminen"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi

Miksi kirjoittaa standardi virheeseen (standard error)? Se on ohjelmoinnin tärkeä osa, jota käytetään virheiden hallintaan ja debuggaamiseen.

## Miten

Seuraavassa on esimerkkejä Elm-koodista ja tulostuksesta. Koodi-kappaleet on merkitty "```Elm ... ```"-merkinnällä.

```Elm
-- Luo virheilmoitus
error "Tämä on virheilmoitus!"

-- Käsittele virheilmoitus
case 1 / 0 of
    Result.Ok result ->
        -- Jos tulos on ok, tulosta se
        Debug.toString result

    Result.Err err ->
        -- Jos tulos on virhe, tulosta virheilmoitus
        error (Debug.toString err)
```

Tulostus:

```
Tämä on virheilmoitus! -- Ensimmäinen esimerkki
"Division by zero error" -- Toinen esimerkki
```

## Syvällisempi tarkastelu

Standardi virheen kirjoittamisella on muutamia huomionarvoisia seikkoja:

- Virheen käsittely tapahtuu `case`-lauseessa, jossa joko `Result.Ok` tai `Result.Err` menestyvät riippuen tuloksesta
- `Debug.toString` muuttaa arvon merkkijonoksi, jotta sen voi tulostaa
- `error`-funktioon voidaan antaa joko merkkijono tai `Debug.toString`-arvo

## Katso myös

- [Error handling in Elm](https://elm-lang.org/docs/error-handling)
- [Elm standard library documentation](https://package.elm-lang.org/packages/elm/core/latest/)