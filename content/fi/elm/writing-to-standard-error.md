---
title:                "Elm: Kirjoittaminen standardievääseen"
programming_language: "Elm"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi kirjoittaa standardiin virhetulosteeseen?

Kirjoittaminen standardiin virhetulosteeseen on tärkeä osa ohjelmoinnin oppimista Elm-kielellä. Se auttaa kehittäjiä ymmärtämään virheitä ja löytämään niiden syyt nopeasti ja tehokkaasti.

## Kuinka kirjoittaa standardiin virhetulosteeseen?

Kirjoittaminen standardiin virhetulosteeseen on helppoa Elm-kielellä. Käytä ```Debug.log``` -funktiota ja lisää haluttu viesti sekä haluamasi muuttujat näyttääksesi virheet ja niiden arvot.

```Elm
Debug.log "Virhe" virheenviesti
```

tulostaa esimerkiksi:

```
Virhe: "Divide by zero"
```

Voit myös käyttää ```Debug.crash``` -funktiota tulostaaksesi virheen halutun arvon kanssa:

```Elm
Debug.crash "Virhe" 500
```

tulostaa:

```
Virhe: 500
```

## Syvemmälle standardiin virhetulosteen kirjoittamiseen

Standardiin virhetulosteen kirjoittaminen voi auttaa myös debuggaamaan virheitä koodissa, jotka eivät anna selkeää virheilmoitusta. Voit käyttää ```elm-repl``` -työkalua tutkiaksesi ja testataksesi koodia vaihe vaiheelta.

Esimerkkejä:

- [Yksinkertainen laskin, joka jakaa nollalla](https://ellie-app.com/7X8zNQzQKzKa1)
- [Funktionaaliset komponentit ja virhetulosteet](https://ellie-app.com/7QCX7Ftj7zwa1)

## Katso myös

- [Elm virheet ja debuggaus](https://guide.elm-lang.org/debugging/errors.html)
- [Debuggaus ohjelmoinnin oppimisen apuna](https://dev.to/lydiahallie/debugging-as-a-learning-tool-3l7d)
- [Ohjelmointivirheiden hallinta ja ennaltaehkäisy](https://codeburst.io/handling-errors-in-programming-with-cdf05e181ea)