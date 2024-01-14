---
title:    "Gleam: Komentoriviparametrien lukeminen"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Miksi

Miksi tulisi lukea komentoriviparametreja? Koska komentoriviparametrit voivat olla hyödyllisiä ohjelman suorittamisen aikana ja tarjoavat enemmän joustavuutta käyttäjille.

## Miten

Gleam tarjoaa helpon ja tehokkaan tavan lukea komentoriviparametreja ohjelmaan. Tässä on muutama esimerkki siitä, miten voit lukea ja käyttää komentoriviparametreja Gleamissa:

```Gleam
import gleam/command_line

// Luetaan yksittäinen parametri ja tallennetaan se muuttujaan "nimi"
nimi = command_line.arguments()[1]

// Tulostetaan viesti käyttäen luettua parametria
io.format("Hei, ~s!", [nimi])

// Luetaan lista parametreja ja tulostetaan ne käyttäen for-loopia
lista = command_line.arguments()
for arg in lista {
    io.format("Komentoriviparametri: ~s", [arg])
}
```

Esimerkkituloste:

```
$ gleam hello_gleam Gleam
Hei, Gleam!
Komentoriviparametri: hello_gleam
Komentoriviparametri: Gleam
```

## Syventyminen

Lisätietoja komentoriviparametrien lukemisesta Gleamissa:

- `command_line.arguments()` palauttaa listan komentoriviparametreista, joista ensimmäinen elementti on itse ohjelman nimi ja loput ovat käyttäjän antamia parametreja.
- `command_line.flag(name)` funktioa voi käyttää tarkistamaan, onko tiettyä parametria annettu komentorivillä vai ei. Se palauttaa boolean-arvon `true` jos parametri on annettu ja `false` jos ei.
- `command_line.arg(name)` funktioa voi käyttää lukemaan tietyn parametrin arvon. Se palauttaa `Option` -tyypin, joka voi olla joko `Some` jos parametri on annettu ja `None` jos ei.
- Voit myös antaa oletusarvoja parametreille käyttäen `command_line.arg_default(name, default_value)` ja voit antaa käyttäjän lisätä parametreja käyttämällä `command_line.more_args()`.

## Katso myös

- [Gleamin komentoriviparametrit -dokumentaatio](https://gleam.run/documentation/gleam_cli_arguments)
- [Gleamin pääsivusto](https://gleam.run/)
- [Gleamin dokumentaatio](https://gleam.run/documentation/)