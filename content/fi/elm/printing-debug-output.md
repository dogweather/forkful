---
title:                "Elm: Tulostetaan virheenjäljitystietoja"
programming_language: "Elm"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä, miksi voit haluta käyttää debuggausta ohjelmoinnissa. Ehkä haluat tarkistaa sovelluksesi sisäisiä tiloja tai löytää vaikeasti havaittavia virheitä. Oli syy mikä tahansa, debuggaus voi olla hyödyllistä ja tehokasta työkalua ohjelmoijille.

## Miten

Elm tarjoaa sisäänrakennetun debuggausominaisuuden, joka helpottaa debuggausta ja mahdollistaa nopeamman ohjelmoinnin. Voit tulostaa debug viestejä käyttäen `Debug.log` funktiota. Se ottaa kaksi argumenttia: merkkijonon ja arvon, jonka haluat tulostaa.

```Elm
Debug.log "Viesti" muuttuja
```

Tämä tulostaa `muuttuja` arvon termiiniprojektiisi. Voit myös käyttää `Debug.log` funktiota tuplakesyhityksessä tarkan debuggauksen helpottamiseksi.

## Syvällinen sukellus

`Debug.log` funktio ei ole ainoastaan kätevä tapa tulostaa arvoja, vaan sillä on myös hyödyllisiä ominaisuuksia. Voit esimerkiksi käyttää sitä tarkistaaksesi, onko tietyn ehtolausekkeen osa suoritettu. Jos ehtolausekkeen ei pitäisi olla osa suoritusta, voit tulostaa virheilmoituksen käyttämällä `Debug.crash` funktiota. Tämä auttaa sinua löytämään virheitä ja korjaamaan ne nopeasti.

## Katso myös

- [Elm-kielen virallinen sivusto](https://elm-lang.org/)
- [Elm-kielen dokumentaatio](https://elm-lang.org/docs)
- [Elm-kielen Slack-yhteisö](https://elmlang.slack.com/archives/C01AFPS347Y)