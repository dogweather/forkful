---
title:                "Kirjoittaminen standardivirheeseen"
date:                  2024-02-03T19:33:05.563852-07:00
model:                 gpt-4-0125-preview
simple_title:         "Kirjoittaminen standardivirheeseen"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?

Virheiden kirjoittaminen standardivirheeseen (stderr) Elixirissä on menetelmä virheilmoitusten ja diagnostiikkatietojen ohjaamiseksi erilleen pääulostulosta (stdout). Ohjelmoijat käyttävät stderr:iä virheiden selvittämiseen ja käsittelyyn sekoittamatta ohjelman pääulostuloa, mikä tekee ongelmien tunnistamisesta ja korjaamisesta helpompaa.

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
