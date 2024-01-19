---
title:                "Komentorivin argumenttien lukeminen"
html_title:           "Bash: Komentorivin argumenttien lukeminen"
simple_title:         "Komentorivin argumenttien lukeminen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Komennon rivin argumenttien lukeminen merkitsee tietojen saamista käyttäjältä ohjelman suorituksen aikana. Ohjelmoijat tekevät tätä voidakseen saada joustavat ja mukautettavat ohjelmat, joita voidaan muuttaa käyttäjän syötteen perusteella.

## Miten tehdä:
Gleam-luettelossa ohjelman argumentit on saatavissa stand-alone `main` -funktion kautta. Tässä on esimerkki koodista ja sen tuottamasta tulosteesta:

```Gleam
import gleam/list 
import gleam/io

fn main(args: list.List(unicode.String)) {
    let args_string = list.join(args, ", ")
    io.println(args_string)
}
```

Suorita tämä ohjelma komentoriviltä ja anna sille muutamia argumentteja. 
Esimerkiksi, jos annat argumentit "eka", "toka"

```
$ gleam run my_app eka toka
```

Ohjelma tulostaa:

```
eka, toka
```

## Syvempi tarkastelu
Komennon rivin argumenttien lukeminen on ollut ohjelmoinnin perusta 1970-luvulta lähtien, jolloin UNIX-käyttöjärjestelmät olivat suosittuja. Vaihtoehtoisesti, interaktiivisessa ohjelmassa voit kysyä syötettä käyttäjältä ohjelmasuorituksen aikana. Lue Gleam-ohjelman argumentit, jotka luodaan `main`-funktion kutsussa, joka on ohjelmasi ensimmäinen kutsuttu funktio.

## Katso myös
Lisätietoja ja esimerkkejä Gleam-koodista voi löytää Gleam-dokumentaatiosta ja sen oppaista.

- Gleam-dokumentaatio: https://gleam.run/book/
- Gleam-opas: https://gleam.run/getting-started/