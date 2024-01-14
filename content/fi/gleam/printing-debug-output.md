---
title:                "Gleam: Virheiden tarkistustulostus"
simple_title:         "Virheiden tarkistustulostus"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi

Oletko ikinä joutunut kamppailemaan koodin kanssa ja toivonut voivasi tarkastella sen sisäistä toimintaa? Tämä on silloin, kun debug-tulosteiden käyttöön otto tulee tarpeeseen. Se antaa sinulle paremman käsityksen siitä, miten koodisi toimii ja auttaa sinua löytämään ja korjaamaan mahdollisia virheitä.

## Kuinka

Gleam tarjoaa helpon tavan lisätä debug-tulosteita koodiisi. Voit käyttää `io.println`-funktiota tulostamaan haluamasi tiedot konsoliin. Voit myös käyttää muuttujien arvoja tai kutsua muita funktioita `io.format`-funktion kautta.

```Gleam
let nimi = "Matti"
io.println("Tervehdys, {nimi}!")
```

Tulostus: `Tervehdys, Matti!`

## Syvemmälle

Debug-tulosteisiin liittyy usein myös joukko arvokkaita toimintoja, kuten logitiedostojen tallentaminen tai virheilmoitusten analysointi. Voit hyödyntää Gleamin vakio-ohjelmistonkirjastoja (stdlib) löytääksesi näille toiminnoille sopivia työkaluja.

Lisäksi voit myös ohjelmoida omia debug-tulosteita varten suoraan Gleamin ominaisuuksien kautta. Tämä saattaa vaatia enemmän työtä, mutta kannattaa tutkia mahdollisuuksia jos haluat räätälöidä debug-tulosteen juuri itsellesi sopivaksi.

## Katso myös

- Gleamin vakio-ohjelmistonkirjaston (stdlib) [dokumentaatio](https://gleam.run/stdlib/)
- ["The Power of Debug Printing in Programming"](https://mattbrinton.com/the-power-of-debug-printing-in-programming/)
- ["Debugging Tooltips for Language Designers"](https://www.infoq.com/presentations/debugging-tooltips-language-designers/) (esitelmä debug-tulosteista)