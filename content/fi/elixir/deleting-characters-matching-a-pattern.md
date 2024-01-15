---
title:                "Kuviota vastaavien merkkien poistaminen"
html_title:           "Elixir: Kuviota vastaavien merkkien poistaminen"
simple_title:         "Kuviota vastaavien merkkien poistaminen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi

Mikä voisi olla syy poistaa merkkejä, jotka vastaavat tiettyä kaavaa? Oikeastaan siinä voi olla monia perusteita. Saattaa olla, että haluat puhdistaa tietoa tai ehkä haluat vain tehdä koodista siistimmän ja helpommin luettavan.

## Näin teet sen

Tarvitsetko poistaa merkkejä, jotka vastaavat tiettyä kaavaa Elixirissä? Ei hätää, sillä siihen on helppo ratkaisu. Voit käyttää Regex-kirjastoa poistamaan halutut merkit. Katso esimerkkiä alla:

```Elixir
Regex.replace("Some text with numbers: 1234 and symbols: !@#$", ~r/[0-9!@#$]/, "")
# Output: "Some text with numbers:  and symbols: "
```

Tämä esimerkki käyttää Regex.replace-funktiota korvaamaan kaikki numerot ja symbolit tyhjällä merkillä. Voit myös käyttää Regex.match-funktiota tarkistaaksesi, että poistanut merkit todella vastaavat haluamaasi kaavaa.

## Syvemmälle aiheeseen

Elixirin Regex-kirjasto tarjoaa monia eri toimintoja, joilla voi käsitellä merkkien kaavoja. Voit esimerkiksi käyttää Regex.split-funktiota jakamaan merkkijonon kaavaa vastaavien merkkien kohdalta. Lisäksi voit käyttää Regex.scan-funktiota löytämään kaavaa vastaavat osat merkkijonosta.

Jos haluat oppia lisää Regex-kirjastosta ja sen eri toiminnoista, suosittelemme lukemaan [virallisen dokumentaation](https://hexdocs.pm/elixir/Regex.html) sekä [Regular-Expressions.info](https://www.regular-expressions.info/).

## Katso myös

- [Regex-kirjaston virallinen dokumentaatio](https://hexdocs.pm/elixir/Regex.html)
- [Regular-Expressions.info](https://www.regular-expressions.info/)