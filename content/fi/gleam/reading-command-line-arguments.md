---
title:                "Gleam: Lukeminen komentoriviparametreista"
simple_title:         "Lukeminen komentoriviparametreista"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi

Miksi lukisin komentoriviparametreja Gleam-ohjelmoinnin avulla? Käytössä on monia erilaisia tapoja, joilla voit optimoida ja tehostaa ohjelmasi toimintaa ja komentoriviparametrit ovat yksi niistä. Tässä blogikirjoituksessa käsittelemme kuinka voit lukea komentoriviparametreja Gleam-ohjelmoinnissa ja miten se voi auttaa sinua ohjelmasi kehittämisessä.

## Kuinka tehdä

Komentoriviparametrien lukeminen Gleamissa on helppoa ja nopeaa. Voit käyttää siihen standardikirjastosta löytyvää `Command.Args` -moduulia. Se tarjoaa kätevän tavan lukea komentoriviltä saadut parametrit ja käsitellä niitä. Käytämme tässä esimerkkinä yksinkertaista ohjelmaa, joka tulostaa käyttäjän antaman nimen tervehdyksenä.

```Gleam
import Gleam.String

pub fn main() {
  let args = Command.Args.parse()
  
  let name = case args {
    command::result(value) -> value
    command::exception(err) -> {
      let _ = erlang::error_logger(format("Virhe: {}", format("{:?}", err)))
      "nimetön"
    }
    _ -> "nimetön"
  }
  
  let greeting = String.concat("Hei ", name, "!")
  erlang::io::format(greeting)
}
```

Käyttäessämme komentoriviparametreja tämän ohjelman kanssa, voimme syöttää nimen parametrina komentoriville seuraavasti:

```
$ hello_world Titi
```

Tämä tulostaa komentorivin seuraavan rivin: `Hei Titi!`.

## Syventävä sukellus

Kuten näemme esimerkissä, `Command.Args` -moduuli tarjoaa kätevän tavan lukea ja käsitellä komentoriviltä saadut parametrit. Voit myös määrittää oletusarvoja, jos käyttäjä ei anna parametria, ja tarkastella tarkemmin virhetilanteita. Lisätietoja tästä moduulista voit löytää Gleamin dokumentaatiosta.

## Katso myös

- [Gleamin dokumentaatio](https://gleam.run/book/getting-started)
- [Erlangin komentorivi- ja argumenttikäsittelyn dokumentaatio](https://erlang.org/doc/man/erl.html#command-line-arguments)