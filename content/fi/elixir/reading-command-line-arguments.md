---
title:                "Komentoriviparametrien lukeminen"
html_title:           "Elixir: Komentoriviparametrien lukeminen"
simple_title:         "Komentoriviparametrien lukeminen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?

Komentoriviparametrien lukeminen on prosessi, jossa ohjelmoija saa käyttäjältä syötettä komentoriviltä, esimerkiksi tietoa tiedostojen poluista tai asetuksista, jota ohjelma käyttää suorituksen aikana. Tämä on tärkeää, jotta ohjelma voi toimia oikein ja käyttäjä voi vaikuttaa sen toimintaan.

## Kuinka:

```Elixir
args = System.argv
IO.puts "Komentoriviparametrit: #{inspect args}"
```
Esimerkissä luodaan muuttuja `args`, joka sisältää käyttäjän antamat komentoriviparametrit. Käyttöjärjestelmän `System` -moduulista kutsutaan `argv` -funktiota, joka palauttaa parametrit listana. Tämän jälkeen kutsutaan `IO.puts` -funktiota, joka tulostaa parametrit konsoliin. 

Tulostus voi näyttää esimerkiksi tältä:
```
Komentoriviparametrit: ["tiedosto1.txt", "-v", "-o", "tiedosto2.txt"]
```

## Syvemmälle:

Yleensä komentoriviparametrit luetaan ohjelman suorituksen alussa, jolloin ne voivat vaikuttaa sen toimintaan. Historiallisesti, komentoriviparametrit ovat olleet tapa määrittää ohjelman suorituksen oletusarvoja tai asetuksia. Nykyään ne voivat myös olla keino välittää käyttäjän antamaa tietoa ohjelmalle.

Vaihtoehtoisena ratkaisuna komentoriviparametreille, ohjelmoijat voivat myös käyttää ympäristömuuttujia, jotka ovat tietoja, jotka asetetaan käyttöjärjestelmän ympäristöön ja joihin ohjelma voi viitata suorituksen aikana.

Elixirissä komentoriviparametrit luetaan `System.argv` -funktiolla, mutta myös `OptionParser` -moduuli tarjoaa mahdollisuuden käsitellä ja parsia parametreja helpommin.

## Katso myös:

- [Elixirin System-moduuli](https://hexdocs.pm/elixir/System.html)
- [Elixirin OptionParser-moduuli](https://hexdocs.pm/elixir/OptionParser.html)
- [Ympäristömuuttujat käyttöjärjestelmässä](https://en.wikipedia.org/wiki/Environment_variable)