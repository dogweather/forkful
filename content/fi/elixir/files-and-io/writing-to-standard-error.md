---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:05.563852-07:00
description: "Miten: Elixiriss\xE4 voit k\xE4ytt\xE4\xE4 `IO`-moduulin funktioita,\
  \ kuten `IO.puts/2` ja `IO.warn/2`, kirjoittaaksesi viestej\xE4 standardivirheeseen."
lastmod: '2024-03-13T22:44:56.243394-06:00'
model: gpt-4-0125-preview
summary: "Elixiriss\xE4 voit k\xE4ytt\xE4\xE4 `IO`-moduulin funktioita, kuten `IO.puts/2`\
  \ ja `IO.warn/2`, kirjoittaaksesi viestej\xE4 standardivirheeseen."
title: Kirjoittaminen standardivirheeseen
weight: 25
---

## Miten:
Elixirissä voit käyttää `IO`-moduulin funktioita, kuten `IO.puts/2` ja `IO.warn/2`, kirjoittaaksesi viestejä standardivirheeseen:

```elixir
# Kirjoittaa yksinkertaisen viestin stderr:iin
IO.puts(:stderr, "Virhe: Jotain meni pieleen!")

# Käyttää IO.warn, joka on semanttisesti sopivampi varoituksille/virheille
IO.warn("Varoitus: Olet ylittämässä rajan!")
```

Esimerkkituloste terminaalissa `IO.puts/2`-käytöllä:
```
Virhe: Jotain meni pieleen!
```

`IO.warn/2`-käytöllä tuloste olisi samankaltainen, mutta `IO.warn/2` on erityisesti suunniteltu varoituksille ja saattaa sisältää lisämuotoilua tai -toimintaa tulevissa Elixir-versioissa.

**Kolmannen osapuolen kirjastojen käyttö**

Vaikka Elixiriin sisältyvä vakio kirjasto on yleensä riittävä käsittelemään standardivirheulostuloa, saatat löytää kirjastoja, kuten `Logger`, hyödyllisiksi monimutkaisemmissa sovelluksissa tai erilaisten lokitasojen ja tulosteiden määrittämiseen.

Esimerkki käyttäen `Logger`ia virheviestin tulostukseen:

```elixir
require Logger

# Konfiguroi Logger ulostamaan stderr:iin
Logger.configure_backend(:console, device: :stderr)

# Kirjoittaa virheviestin
Logger.error("Virhe: Yhteys tietokantaan epäonnistui.")
```

Tämä asetus ohjaa `Logger`in tulosteen erityisesti stderr:iin, mikä on hyödyllistä erottaessasi virhelokit tavallisista lokiviesteistä.
