---
date: 2024-01-26 01:02:40.962528-07:00
description: "Miten: Elixiriss\xE4 perustavanlaatuinen tapa lokitietojen kirjaamiseen\
  \ on sis\xE4\xE4nrakennetun `Logger`-moduulin kautta. T\xE4ss\xE4 on ohjeet sen\
  \ k\xE4ytt\xF6\xF6n."
lastmod: '2024-03-13T22:44:56.233467-06:00'
model: gpt-4-1106-preview
summary: "Elixiriss\xE4 perustavanlaatuinen tapa lokitietojen kirjaamiseen on sis\xE4\
  \xE4nrakennetun `Logger`-moduulin kautta."
title: Lokitus
weight: 17
---

## Miten:
Elixirissä perustavanlaatuinen tapa lokitietojen kirjaamiseen on sisäänrakennetun `Logger`-moduulin kautta. Tässä on ohjeet sen käyttöön:

```elixir
defmodule MyApplication do
  require Logger

  def do_something_important(param) do
    Logger.info("Aloitetaan tärkeä prosessi parametrilla: #{param}")

    # Simuloi työn tekemistä
    :timer.sleep(1000)

    Logger.debug("Prosessi suoritettu.")
  rescue
    error -> Logger.error("Tapahtui virhe: #{inspect(error)}")
  end
end

# Nähdäksesi lokitiedot, sinun tulee vain kutsua funktiota:
MyApplication.do_something_important("MyParam")
```

Tämä yksinkertainen koodinpätkä näyttää, miten logataan eri tasolla (`info`, `debug` ja `error`). Kun suoritat tämän, et näe debug-viestiä, ellet konfiguroi Logger-tasoa `:debug`-arvoon. Oletuksena Elixiriin sisäänrakennettu Logger suodattaa pois lokiviestit, jotka ovat alempia kuin `:info`.

Esimerkkitulostus `:info`-tasolla voisi näyttää tältä:
```
14:32:40.123 [info]  Aloitetaan tärkeä prosessi parametrilla: MyParam
14:32:41.126 [error] Tapahtui virhe: %RuntimeError{message: "suoritusaikainen virhe"}
```

## Syväsukellus:
Elixirin `Logger` on sisäänrakennettu työkalu, joka on ollut osa kieltä sen alkuaikoina. Se on saanut vaikutteita muiden BEAM-kielten, kuten Erlangin, lokitusjärjestelmistä. Logger tarjoaa eri lokitustasoja – `:debug`, `:info`, `:warn` ja `:error` – ja se on laajennettavissa, mikä mahdollistaa erilaisten taustajärjestelmien liittämisen lokiviestien käsittelyyn.

Yksi vaihtoehto sisäänrakennetulle Loggerille monimutkaisemmissa tilanteissa on lokituskirjastojen, kuten `Logstash` tai `Sentry` Elixirille, käyttö. Nämä voivat tarjota lisäominaisuuksia, kuten virheiden seurannan ja kokoamisen visuaalisessa muodossa. Paikallisessa kehitystyössä Elixirin kehittäjät luottavat usein Loggerin sisäänrakennettuun toiminnallisuuteen sen yksinkertaisuuden ja BEAM-VM:n integraation vuoksi.

Sisäisesti Logger-moduuli tarjoaa asynkronista ja synkronista lokitusta. Asynkroninen lokitus, joka on oletusarvo, ei estä sovelluksesi suorittamista viestien lokitessa. Tämä varmistaa, että lokitus ei vaikuta negatiivisesti suorituskykyyn. Synkroninen lokitus voidaan kuitenkin ottaa käyttöön tapauksissa, joissa on taattava viestien logittaminen siinä järjestyksessä, kun ne on lähetetty.

Loggerin konfiguraatiota voidaan säätää Elixir-sovelluksen `config/config.exs`-tiedostossa, jossa voit asettaa lokitustason, muodon, metatiedot ja muuta. Muista aina säätää lokitustasosi ja -tulosteesi eri ympäristöihin; et halua, että tuotantojärjestelmäsi tulvitaan pitkäpiirteisillä debug-lokeilla.

## Katso myös:
- Virallinen Elixirin Logger-dokumentaatio: https://hexdocs.pm/logger/Logger.html
- Blogikirjoitus parhaista Elixirin lokituskäytännöistä: https://blog.appsignal.com/2020/05/06/elixir-logging-tips-and-tricks.html
- Sentry Elixirille Hex:ssä: https://hex.pm/packages/sentry
- Elixir Schoolin oppitunti Loggerista: https://elixirschool.com/en/lessons/specifics/debugging/#logging
