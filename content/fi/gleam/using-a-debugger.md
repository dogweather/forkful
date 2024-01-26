---
title:                "Debuggerin käyttö"
date:                  2024-01-26T03:50:18.906348-07:00
model:                 gpt-4-0125-preview
simple_title:         "Debuggerin käyttö"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/using-a-debugger.md"
---

{{< edit_this_page >}}

## Mikä ja miksi?
Debuggerin käyttäminen on käytännössä kuin detektiivina toimimista omassa koodissasi, etsien bugeja ja selvittäen, miksi asiat eivät suju sulavasti. Ohjelmoijat tekevät niin, koska, kasvotusten sanottuna, bugit ovat väistämättömiä, ja niiden tehokas tuhoaminen tarkoittaa koodisi saamista toimimaan nopeammin ja luotettavammin.

## Kuinka:
Gleam tukeutuu tällä hetkellä Erlangin ekosysteemiin työkalujen osalta, joten tyypillisesti debuggaat työkaluja kuten `rebar3`, `observer` ja `debugger` käyttäen. Tässä on, miten pääset syvälle ja likaiseen debuggaamiseen:

```gleam
// Varmista rebar-konfiguraatiossasi, että sinulla on nämä rivit sisällyttämässä debug-tietoa:
{erl_opts, [debug_info]}.

// Suorita Erlang-kotelo sovelluksellasi ladattuna
rebar3 shell

// Kotelon sisällä voit käynnistää debuggerin
1> debugger:start().
```

Yksinkertaista, eikö? `debugger` GUI avautuu, ja voit asettaa katkaisupisteitä, käydä koodin läpi askel askeleelta ja tarkkailla muuttujia niin paljon kuin haluat. Et näe suoraan Gleam-koodia, mutta näet Erlang-koodin, johon se kääntyy, mikä on silti aika hyödyllistä.

## Syväsukellus
Gleam on nuori kieli, joten vaikka se seisoo Erlangin ekosysteemin hartioilla, natiivit Gleam-debuggaustyökalut eivät vielä ole parrasvaloissa. Tämä tarkoittaa, että käytämme Erlangin hyväksi koeteltuja työkaluja, eikä se ole huono asia. Erlangin debugger on ollut olemassa jo '90-luvulta lähtien, vuosien ajan hienosäädetty tuhoamaan ärsyttäviä bugeja järjestelmissä, joissa luotettavuus on avainasemassa.

Vaihtoehtojen osalta, jäljitys on tehokas menetelmä BEAM-maailmassa (se on virtuaalikone, joka suorittaa Erlang- ja Elixir-koodia). Käyttäen `rebar3` voit hyödyntää työkaluja kuten `recon` jäljittämään funktiokutsuja ja sukeltaa syvälle suorituskykyongelmiin.

Vaihto Gleamin kirjoittamisen ja Erlangissa debuggaamisen välillä voi tuntua kuin kääntäisit ajatuksiasi lennossa. Mutta etuna on, että saat kurkistuksen Erlangin maailmaan, ymmärtäen sovelluksesi rakennuspalikoita sen suoritusmuodossa.

## Katso myös
Laajentaaksesi debuggaustyökalupakettiasi, tutustu:

- Erlangin debugger-dokumentaatioon: [https://erlang.org/doc/apps/debugger/debugger_chapter.html](https://erlang.org/doc/apps/debugger/debugger_chapter.html)
- Erlangin `recon`-kirjastoon: [https://ferd.github.io/recon/](https://ferd.github.io/recon/)
- Jäljitykseen BEAM:ssa: [https://adoptingerlang.org/docs/development/tracing/](https://adoptingerlang.org/docs/development/tracing/)