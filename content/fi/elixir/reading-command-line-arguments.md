---
title:                "Elixir: Komentoriviparametrien lukeminen"
simple_title:         "Komentoriviparametrien lukeminen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi lukea komentoriviparametrejä

Komentoriviparametrit ovat tärkeitä monissa Elixir-ohjelmissa, koska ne antavat mahdollisuuden tarjota lisätoimintoja ohjelman käyttäjille. Lukemalla komentoriviparametrejä voit tehdä ohjelmasi vieläkin monipuolisemmaksi ja helpommaksi käyttää. Näin voit parantaa käyttäjäkokemusta ja tarjota enemmän vaihtoehtoja ohjelman käyttäjille.

## Kuinka lukea komentoriviparametrejä

Komentoriviparametrien lukeminen Elixirillä on helppoa. Voit käyttää ```OptionParser```-moduulia, joka tarjoaa valmiin tavan lukea ja käsittellä parametrejä. Voit aloittaa luomalla uuden ```OptionParser```-olion ja määrittää sille haluamasi parametrit.

```
# Luodaan uusi OptionParser-olio
parser = OptionParser.new

# Lisätään parametri "username"
parser.add(:username, "Käyttäjänimi")

# Lisätään parametri "autocomplete", joka ottaa vastaan Boolean-arvon
parser.add(:autocomplete, "Automaattinen täydennys", types: [boolean])

# Luetaan parametrit käyttäjältä
options = parser.parse(ARGV)
```

Kun olet lisännyt kaikki tarvittavat parametrit ja lukenut ne käyttäjältä, voit käyttää ```options```-muuttujaa saadaksesi parametrien arvot.

```
# Saadaan käyttäjänimi
username = options[:username]

# Saadaan boolean-arvo automaattiselle täydennykselle
autocomplete = options[:autocomplete]
```

Voit myös määrittää oletusarvoja parametreille ja käyttää ehtolauseita parametrien käsittelyyn.

## Syvällinen sukellus komentoriviparametreihin

Komentoriviparametrien lukeminen ```OptionParser```-moduulilla on erittäin hyödyllistä, sillä se tarjoaa valmiin tavan lukea ja käsittellä parametrejä. Voit myös käyttää muita tapoja lukea parametreja, kuten käyttämällä ```System.argv```.

On tärkeää huomata, että komentoriviparametrien lukeminen ei ole aina välttämätöntä, mutta se voi tehdä ohjelmastasi käyttäjäystävällisemmän ja lisätä sen monipuolisuutta.

## Katso myös

- [Elixirin virallinen dokumentaatio OptionParserista](https://hexdocs.pm/elixir/OptionParser.html)
- [Elixir Forum: How to read command line arguments](https://elixirforum.com/t/how-to-read-command-line-arguments/8217)
- [Medium: Introduction to Elixir command line argument parsing](https://medium.com/@noahhlw/introduction-to-elixir-command-line-argument-parsing-b09b91d8c108)