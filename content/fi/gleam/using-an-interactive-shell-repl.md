---
title:                "Interaktiivisen komentotulkin (REPL) käyttö"
date:                  2024-01-26T04:14:21.584389-07:00
model:                 gpt-4-0125-preview
simple_title:         "Interaktiivisen komentotulkin (REPL) käyttö"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

REPL, lyhenne sanoista Read-Eval-Print Loop (Lue-Arviointi-Tulosta Silmukka), on ohjelmointityökalu koodin interaktiiviseen suorittamiseen ja tulosten heti näkemiseen. Ohjelmoijat käyttävät sitä kokeiluihin, vianmääritykseen tai uuden kielen, kuten Gleamin, oppimiseen lennosta.

## Kuinka:

Gleam ei tällä hetkellä sisällä REPL:ää vakiojakelussaan. Voit kuitenkin kokeilla Gleam-koodia käyttäen olemassa olevaa Erlang-komentokehotetta, koska Gleam kääntyy Erlangin tavukoodiksi. Näin teet sen:

1. Käännä Gleam-koodisi Erlangiksi.
```plaintext
gleam build
```

2. Käynnistä Erlang-komentokehote.
```plaintext
erl -pa ebin
```

3. Kutsu Gleam-funktioitasi (olettaen, että sinulla on moduuli nimeltä `my_mod` ja funktio `my_fun`).
```erlang
my_mod:my_fun().
```

Sinun pitäisi nähdä funktion tuloste komentokehotteessa.

## Syväsukellus

REPL heijastaa monien funktio-ohjelmointikielien dynaamista ja tutkivaa henkeä, joka juontaa juurensa 1960-luvun LISP:n REPL:ään. Vertailukohdaksi, muut järjestelmät kuten Pythonin `ipython` tai Rubyn `irb` tarjoavat samanlaisia kokemuksia yhteisöilleen.

Vaikka Gleamilla ei vielä ole natiivia REPL:ää, Erlang-komentokehotteen käyttö on nokkela kiertotapa. Erlang-komentokehotteen kyvyt tulevat BEAM VM:stä, virtuaalikoneesta, joka voimaa Erlang-ekosysteemin, johon kuuluvat Elixir, LFE ja Gleam.

Vaihtoehtoja REPL:ille Gleam-ekosysteemissä voivat sisältää testitapausten kirjoittamisen tai online-kääntäjien ja koodileikkikenttien käytön, jotka tukevat Gleamia, koodinpätkien testaamiseen kokonaisen projektiasetuksen ulkopuolella.

Dedikoidun Gleam REPL:n toteuttaminen kohtaa haasteita pääasiassa Gleamin ja Erlangin ajonaikaisen ympäristön käännetyyn luonteeseen liittyen, jossa koodin kuuma vaihto on normi. Mahdollisen tulevan Gleam REPL:n tulisi sovittaa yhteen kielen staattinen tyypitys ja dynaamisen suoritusympäristön, jonka REPL odottaa.

## Katso Myös

- Gleamin virallinen dokumentaatio: https://gleam.run/book/
- Erlangin komentokehotteen dokumentaatio: http://erlang.org/doc/man/erl.html
- Online Gleam-kääntäjän leikkikenttä: https://gleam.run/compiler/