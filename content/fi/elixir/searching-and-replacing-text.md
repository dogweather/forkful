---
title:                "Tekstin etsiminen ja korvaaminen"
html_title:           "Elixir: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

Mikä & Miksi?
Tekstin hakeminen ja korvaaminen on prosessi, jossa etsitään ja muutetaan tiettyjä osia tekstistä. Ohjelmoijat käyttävät tätä työkalua esimerkiksi korjatessaan kirjoitusvirheitä tai vaihtaessaan tiettyjä sanoja tai lauseita.

Miten tehdä:
Elixirin avulla tekstien hakeminen ja korvaaminen on yksinkertaista. Käytämme tässä esimerkkinä yksinkertaista tekstin korvaamista. Voit käyttää funktiota `String.replace/3` ja antaa sille kaksi merkkijonoa ja korvaavan arvon. Alla on esimerkki:

```Elixir
iex> String.replace("Tervetuloa maailma", "maailma", "Elixir")
"Tervetuloa Elixir"
```

Syötteeksi annettu merkkijono ei muutu, vaan funktio luo uuden merkkijonon halutuin muutoksin.

Syväsukellus:
Tekstin hakeminen ja korvaaminen on ollut pitkään tärkeä osa ohjelmointia. Ennen Elixirin kaltaisia kieliä, tekstien hakeminen ja korvaaminen vaativat monimutkaisempia toimenpiteitä. Nykyään monet ohjelmointikielet tarjoavat erilaisia ​​funktioita tekstin käsittelyyn, mutta Elixirin tapa on yksi helpoimmista ja tehokkaimmista.

Katso myös:
Lisätietoa tekstien hakemisesta ja korvaamisesta Elixirilla löytyy kielen virallisilta verkkosivuilta osoitteesta https://elixir-lang.org/getting-started/io-and-the-file-system.html#hakeminen-ja-korvaaminen.

Jos haluat oppia lisää Elixirin tekstin käsittelystä, voit lukea virallisen dokumentaation Elixirin `String`-moduulista https://hexdocs.pm/elixir/String.html.