---
title:    "Gleam: Virheenkorjaustulosteen tulostaminen"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Miksi

Miksi kukaan edes harkitsisi debug-tulosteiden tulostamista Gleam-koodissa? Toki, jos kaikki koodissa toimii täydellisesti ja virheet ovat vain urbaanilegendoja, silloin ei tätä tarvitsekaan miettiä. Mutta jos kuitenkin jostain syystä (vai johtuuko sittenkin viallisesta kahvinkeittimestä?) ohjelman suoritukseen ilmestyy odottamattomia ongelmia, debug-tulosteiden printtaaminen voi helpottaa virheen etsimistä ja korjaamista.

## Kuinka

Eli miten tulostat debug-tulosteita Gleam-koodissa? No, se on todella yksinkertaista! Sinun tarvitsee vain käyttää "`` `Gleam`` `" -koodilohkoa ja "`` `io`` `" -moduulia.

Tässä on esimerkki debug-tulosteen printtaamisesta:

```
Gleam
import io
pub fn main() {
  io.debug("Debug-tuloste")
}
```

Ja tässä on tulos:

```
Debug-tuloste
```

Kuten huomaat, "`` `io`` `" -moduuli tarjoaa `debug` -funktion, joka ottaa vastaan haluamasi tekstin parametrina ja printtaa sen terminaaliin. Lisäksi voit käyttää `debug` -funktiota myös muuttujien printtaamiseen, vain lisää muuttuja tekstin loppuun:

```
Gleam
import io
alias Show = io.debug
pub fn main() {
  let nimi = "Kalle"
  Show("Tervehdys ", nimi)
}
```

Ja tässä tulos:

```
Tervehdys Kalle
```

## Syvemmälle

Jos haluat mennä vielä syvemmälle debug-tulosteiden printtaamisessa, voit myös käyttää `inspect` -funktiota. Se näyttää tarkemman kuvauksen tulosteen sisällöstä, kuten esimerkiksi tietotyypin ja sen sisällön:

```
Gleam
import io
alias Show = io.inspect
pub fn main() {
  let numero = 42
  let totuusarvo = true
  let nimi = "Maikki"
  Show(numero, totuusarvo, nimi)
}
```

Ja tulos on:

```
{int, 42}
{bool, true}
{string, "Maikki"}
```

Tämäkin on yksi tapa tarkastella ohjelman suorituksessa esiintyviä muuttujia ja arvoja.

## Katso myös

Kun olet saanut debug-tulosteet printattua ja tulkitsemisesta on tullut sinulle arkipäivää, voit tutustua myös muihin Gleamin debuggausmahdollisuuksiin:

- [Virheiden käsittely](https://gleam.run/book/tutorials/error_handling.html)
- [Loggauksen käyttöönotto](https://gleam.run/book/tutorials/logger.html)
- [Debuggerin käyttöönotto](https://gleam.run/book/tutorials/debugger.html)