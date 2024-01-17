---
title:                "Virheenkorjaustulosteen tulostaminen"
html_title:           "Elixir: Virheenkorjaustulosteen tulostaminen"
simple_title:         "Virheenkorjaustulosteen tulostaminen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## Mitä ja Miksi?

Debuggaustulostuksen tulostaminen tarkoittaa ohjelman suorituksen aikana tapahtuvien tapahtumien näyttämistä koodin seassa. Se on tärkeä osa ohjelmointiprosessia, joka auttaa löytämään virheitä ja parantamaan koodin toimivuutta.

## Miten:

 ```Elixir
IO.puts "Koodin tämänhetkinen tila on: #{variable}"
```
Tämä tulostaa "Koodin tämänhetkinen tila on: [muuttujan arvo]" konsolille, jolloin voit seurata ohjelman suoritusta ja tarkistaa muuttujien arvoja.

## Syväsukellus:

Debuggaustulostus on ollut käytössä jo vuosien ajan, ja se on edelleen tärkeä työkalu ohjelmien kehittämisessä. Vaihtoehtoisia tapoja debuggaustulostusten käyttöön ovat esimerkiksi kirjastot, kuten "Logger", jotka tarjoavat joustavampia ja monipuolisempia ratkaisuja.

Elixirissä debuggaustulostukseen käytetään usein IO:n funktioita, kuten "IO.puts" ja "IO.inspect". Nämä toimivat hyvin yksinkertaisissa tilanteissa, mutta monimutkaisemmissa tapauksissa voidaan joutua turvautumaan Loggeriin tai muuhun kirjastoon.

## Nähdäksesi myös:

Lisätietoa debuggaustulostuksesta löydät virallisesta Elixir-dokumentaatiosta: https://hexdocs.pm/elixir/IO.html

Voit myös tutustua vaihtoehtoisiin tapoihin käyttää debuggaustulostuksia Elixirissä, kuten "Logger": http://www.erlang.org/doc/man/logger.html