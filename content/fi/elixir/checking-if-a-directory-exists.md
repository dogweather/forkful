---
title:                "Tarkistetaan, onko hakemisto olemassa."
html_title:           "Elixir: Tarkistetaan, onko hakemisto olemassa."
simple_title:         "Tarkistetaan, onko hakemisto olemassa."
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?

Tarkistaminen, onko hakemisto olemassa, on yksinkertainen, mutta tärkeä osa ohjelmoinnin prosessia. Tällä toimenpiteellä varmistetaan, että ohjelma voi käsitellä olemassa olevia tiedostoja ja hakemistoja asianmukaisesti. Tämä auttaa myös estämään mahdollisia virheitä ja häiriöitä.

## Miten:

```Elixir
IO.puts(File.dir?("hakemisto"))
```
Tämä koodinpätkä kertoo meille, onko "hakemisto" olemassa vai ei ja tulostaa joko true tai false, riippuen vastauksesta.

```Elixir
hakemisto = "hakemisto"
if File.dir?(hakemisto) do 
  IO.puts("Hakemisto #{hakemisto} on olemassa.") 
else 
  IO.puts("Hakemistoa #{hakemisto} ei löydy.") 
end
```

Tässä esimerkissä olemme tallentaneet hakemiston nimen muuttujaan ja käyttäneet sitä ehtolauseessa tulostamaan vastaavan viestin.

## Syväsukellus:

Historiallisessa kontekstissa, hakemistojen tarkistamisen tarve on noussut esiin erityisesti kehitysvaiheessa, kun ohjelmoijat kohtaavat usein erilaisia tiedosto- ja hakemistorakenteita. Tämän toiminnon lisäksi on myös muita tapoja käsitellä tiedostoja ja hakemistoja, kuten "File.exists?" toiminto. Hakemistojen tarkistamisessa käytetään usein myös ehtolauseita ja lohkolauseita, kuten edellä olevassa koodinpätkässä, jotta ohjelma voi suorittaa tarvittavat toimenpiteet halutun tuloksen saavuttamiseksi.

## Katso myös:

- Elixirin virallinen dokumentaatio tiedostojen ja hakemistojen käsittelyyn: https://elixir-lang.org/getting-started/io-and-the-file-system.html
- "Directory exists?" Stack Overflow-sivustolta: https://stackoverflow.com/questions/19349580/how-to-check-if-a-directory-exists-in-elixir