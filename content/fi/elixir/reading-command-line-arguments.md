---
title:                "Elixir: Pääkomennon argumenttien lukeminen"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi: 

Jos haluat oppia paremmin hallitsemaan ohjelmiasi, on tärkeää ymmärtää kuinka luet komentoriviparametrejä. Tämä auttaa sinua luomaan dynaamisempia ja monipuolisempia ohjelmia Elixir-ohjelmointikielen avulla.

## Kuinka:

Komentoriviparametrien lukeminen Elixirissa on helppoa ja suoraviivaista. Käytetään esimerkkinä yksinkertaista ohjelmaa, joka tulostaa käyttäjän antaman nimen komentoriviltä.

```
Elixir yksinkertainen.exs Saija

nimi = IO.argv |> hd()
IO.puts "Hei, #{nimi}!"
```

Käynnistä ohjelma komentoriviltä komennolla ```elixir yksinkertainen.exs Saija```. Tämä tulostaa "Hei, Saija!" konsoliin.

## Syväkellunta:

Kun ajamme edellä esitetyn yksinkertaisen ohjelman, huomaamme että commantoriviparametrit tallennetaan Elixirin ```IO.argv``` muuttujaan. Tämä muuttuja on lista, joten voi olla hyödyllistä käyttää Elixirin listafunktioita, kuten ```head()```, ```tail()```, ```length()``` jne.

Voit myös lisätä ehtoja ohjelmaan, jotta voit suorittaa erilaisia toimintoja eri komentoriviparametrien avulla.

Esimerkiksi, voit tarkistaa onko parametrien lukumäärä oikea ennen kuin suoritat tiettyjä toimintoja. Voit myös käyttää komentoriviparametreja ohjelman asetusten määrittämiseen, kuten tiedostonimet tai käyttäjän antamat asetukset.

## Katso myös:

- [Elixirin IO-moduuli](https://hexdocs.pm/elixir/IO.html)
- [Elixirin listaoperaatiot](https://elixir-lang.org/getting-started/keywords-and-maps.html#list-operations)
- [Komentoriviparametrien käsittely Elixirissa](https://dev.to/yyiki/komentoriviparametrien-kaesittely-elixirissa-26op)
- [Elixirin virallinen sivusto](https://elixir-lang.org/)