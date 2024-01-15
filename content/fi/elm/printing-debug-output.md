---
title:                "Tulostaminen debug-ulos-tulo"
html_title:           "Elm: Tulostaminen debug-ulos-tulo"
simple_title:         "Tulostaminen debug-ulos-tulo"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

# Miksi käyttää debuggaustulosteita?

Debuggaustulosteet ovat todella hyödyllisiä työkaluja koodin vianetsintään. Ne auttavat löytämään puuttuvia tai virheellisiä algoritmeja ja suorittamaan koodin tehokkaammin. Debuggaustulosteiden käyttäminen voi myös säästää paljon aikaa ja vaivaa, sillä se auttaa vianetsinnässä ja koodin korjaamisessa nopeammin.

# Näin käytät debuggaustulosteita Elm-ohjelmoinnissa

Debuggaustulosteiden käyttäminen Elm-ohjelmoinnissa on helppoa ja nopeaa. Voit käyttää `Debug.log` -funktiota tulostamaan haluamasi muuttujan arvon. Alla on esimerkki koodista ja sen tuottamasta tulosteesta:

```elm
import Debug exposing (log)

main =
  let
    x = 5
    y = 10
    sum = x + y
  in
    log "summa" sum
```

Tämän koodin tuloste on `summa = 15`, joten voit nähdä, että muuttujat `x` ja `y` on laskettu ja summa on tallennettu muuttujaan `sum`.

Voit myös käyttää `Debug.todo` -funktiota saadaksesi ilmoituksen, kun ohjelmasi käyttää vielä toteuttamattomia osia. Tämä auttaa sinua muistamaan tulevia tehtäviä ja pitämään ohjelmasi kehityksen ajan tasalla. Alla esimerkki koodista ja sen tuottamasta tulosteesta:

```elm
import Debug exposing (todo)

main =
  let
    name = "Elm"
    message = "Tervehdys " ++ name ++ "!"
    todo = "Implementoi " ++ name ++ "-sovelluksen ulkoasu."
  in
    todo "todo" todo
```

Tuloste tästä koodista on `todo = Implementoi Elm-sovelluksen ulkoasu.`, joten voit muistaa keskittyä tähän tehtävään myöhemmin.

# Syvällisempää tietoa debuggaustulosteista

Debuggaustulosteet voidaan lisätä lähes mihin tahansa kohtaan koodia, joten voit helposti seurata koodin suorittamista ja tulostaa haluamiasi arvoja. On tärkeää huomata, että `Debug` -moduuli pitäisi aina poistaa ennen julkaisua, koska se ei ole tarkoitettu tuotantokäyttöön. Se voi heikentää ohjelman suorituskykyä ja vuotaa luottamuksellisia tietoja. Lisäksi, jos olet kiinnostunut debuggaustulosteiden lisäämisestä HTTP-pyyntöihin, kannattaa tutustua pakettiin `elm-http-extras`, joka tarjoaa lisää hyödyllisiä debuggaustyökaluja.

# Katso myös

- [Elm Dokuwiki: Debug module](https://package.elm-lang.org/packages/elm/core/latest/Debug)
- [Elm Dokuwiki: Todo module](https://package.elm-lang.org/packages/elm/core/latest/Debug-Todo)
- [Elm Dokuwiki: elm-http-extras package](https://package.elm-lang.org/packages/mpizenberg/elm-http-extras/latest/)