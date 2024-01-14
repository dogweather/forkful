---
title:    "Fish Shell: Virheenkorjaustulosteen tulostus"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Miksi

Monet ohjelmoijat käyttävät vianetsintään ja ohjelmien toiminnan tarkistamiseen debug outputia. Tämä mahdollistaa helpomman ja nopeamman kehityksen ja auttaa löytämään virheitä ja parantamaan koodin suorituskykyä.

## Miten

Fish Shellilla voit tulostaa debug outputia käyttäen komentoa `echo`. Voit lisätä haluamasi tiedon tai muuttujan `echo` komennon perään ja se tulostuu terminaalin konsoliin.

```Fish Shell
echo "Debug output on kätevä tapa tarkistaa koodin suorituskykyä"
```

```
Debug output on kätevä tapa tarkistaa koodin suorituskykyä
```

## Syvemmälle

Jos haluat nähdä lisätietoja debug outputista, voit käyttää komentoa `set -x`. Tämä lisää debug outputin kunkin komennon eteen.

```Fish Shell
set -x
echo "Tämä on debug output"
```

```
+ echo "Tämä on debug output"
Tämä on debug output
```

Voit myös käyttää `printf` komentoa, joka vastaa `echo` komentoa, mutta antaa enemmän mahdollisuuksia muotoiluun ja vaihtoehtoisiin tietoihin.

```Fish Shell
printf "Luku %d ja merkkijono %s" 123 "Fish Shell"
```

```
Luku 123 ja merkkijono Fish Shell
```

## Katso myös

- [Fish Shellin viralliset dokumentaatiot](https://fishshell.com/docs/current/)
- [Vianetsintä Fish Shellilla](https://fishshell.com/docs/current/tutorial.html#debugging-your-functions)