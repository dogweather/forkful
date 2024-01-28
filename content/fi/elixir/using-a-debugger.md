---
title:                "Debuggerin käyttö"
date:                  2024-01-26T03:48:25.686817-07:00
model:                 gpt-4-0125-preview
simple_title:         "Debuggerin käyttö"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/using-a-debugger.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Debuggerin käyttäminen Elixirissä sisältää koodisi läpikäymistä askel askeleelta, muuttujien tarkastelua ja virtojen seurantaa virheiden korjaamiseksi. Ohjelmoijat tekevät sen ymmärtääkseen odottamatonta ja varmistaakseen, että heidän sovelluksensa toimivat suunnitellusti.

## Kuinka:
Elixir sisältää sisäänrakennetun graafisen debuggerin nimeltään `:debugger`. Käyttääksesi sitä, sinun tarvitsee käynnistää se ja liittää se käynnissä olevaan prosessiisi.

Ensimmäiseksi, varmista, että `:debugger` on käynnistetty `iex`-istunnossa:
```elixir
iex> :debugger.start()
{:ok, #PID<0.108.0>}
```

Nyt, tulkkaa koodimoduuli, jota haluat debugata:
```elixir
iex> :int.ni(MyApp.MyModule)
{:module, MyApp.MyModule}
```

Voit asettaa katkaisukohdan:
```elixir
iex> :int.break(MyApp.MyModule, line_number)
:ok
```

Ja sitten, suorita funktiosi osuaksesi katkaisukohtaan ja käydä läpi koodiasi:
```elixir
iex> MyApp.MyModule.my_function(arg1, arg2)
# Debugger keskeyttää suorittamisen rivillä, jossa katkaisukohta on
```

## Syväsukellus
Ennen Elixiriä `:debugger`, käytti Erlang tarjotakseen debuggerin, jota Elixir käyttää; se on vankka ja erinomainen käsittelemään samanaikaisia prosesseja, Erlang VM:n (BEAM) vahvuusalue. Toisin kuin jotkin muut debuggerit, `:debugger` ei salli muuttujien muokkaamista lennosta, johtuen Elixiriin datasta joka on muuttumaton. Vaihtoehtoista apua tarjoaa `IEx.pry`, joka mahdollistaa suorituksen keskeyttämisen ja hyppäämisen REPL:iin missä tahansa koodissasi, mikä voi olla erittäin kätevää.

Vaikka `:debugger` on hyvä graafiselle käyttöliittymälle, jotkut saattavat pitää enemmän sisäänrakennetusta `:observer`-työkalusta, joka tarjoaa myös prosessin tarkastelua ja järjestelmämittareita, vaikkei se erityisesti tähtääkään koodin läpikäymiseen. Elixiriin yhteisö myös osallistuu työkaluja kuten `visualixir` ja `rexbug`, laajentaen debuggaustyökalujen ekosysteemiä oletusten ulkopuolelle.

## Katso myös
- Virallinen Elixirin pääsyoppaan debuggauksen aloitusopas: https://elixir-lang.org/getting-started/debugging.html
- Erlangin `:debugger` Dokumentaatio: http://erlang.org/doc/apps/debugger/debugger_chapter.html
- Elixir Forumin keskustelut debuggaustekniikoista: https://elixirforum.com/c/elixir-questions/elixir-questions-questions-help/15
