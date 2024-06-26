---
date: 2024-01-26 03:44:01.585967-07:00
description: "Kuinka: Elixiriss\xE4 voit k\xE4ytt\xE4\xE4 `Float.round/2` funktiota\
  \ liukuluvun py\xF6rist\xE4miseen. Voit m\xE4\xE4ritt\xE4\xE4, kuinka monta desimaalia\
  \ haluat s\xE4ilytt\xE4\xE4. N\xE4in se\u2026"
lastmod: '2024-03-13T22:44:56.221510-06:00'
model: gpt-4-0125-preview
summary: "Elixiriss\xE4 voit k\xE4ytt\xE4\xE4 `Float.round/2` funktiota liukuluvun\
  \ py\xF6rist\xE4miseen."
title: "Numerojen py\xF6rist\xE4minen"
weight: 13
---

## Kuinka:
Elixirissä voit käyttää `Float.round/2` funktiota liukuluvun pyöristämiseen. Voit määrittää, kuinka monta desimaalia haluat säilyttää. Näin se toimii:

```elixir
# Pyöristä luku ilman desimaaleja
Float.round(3.14159) # => 3.0

# Pyöristä luku 2 desimaalin tarkkuuteen
Float.round(3.14159, 2) # => 3.14

# Pyöristä luku negatiivisella tarkkuudella lähimpään 10:een
Float.round(123.456, -1) # => 120.0
```

## Syventävä osio
Numeroiden pyöristäminen on klassinen ongelma tietojenkäsittelytieteessä—siinä määrin, että pyöristysstrategian valinta voi vaikuttaa rahoitusjärjestelmiin, tieteellisiin laskelmiin ja muuhun. Elixiriin `Float.round/2` oletusarvo on "half up" pyöristys, joka muistuttaa perinteistä matematiikan tunneilla opetettua pyöristystä.

Jos tarvitset muita pyöristystyyppejä, Elixir antaa sinun toteuttaa omia. Harkitse esimerkiksi "floor" pyöristystä (aina alas) tai "ceiling" pyöristystä (aina ylös). Käyttäisit `Float.floor/1` tai `Float.ceil/1`, vastaavasti.

```elixir
# Floor pyöristys
Float.floor(3.999) # => 3.0

# Ceiling pyöristys
Float.ceil(3.001) # => 4.0
```

Nämä vaihtoehdot auttavat räätälöimään pyöristämisen sovelluksesi tarkkoihin tarpeisiin, olipa kyse sitten rahoituslaskelmista, grafiikan renderöinnistä tai datan likiarvosta.

## Katso myös
Lisätietoja Elixiriin pyöristysfunktioista ja liukuluvuista:

- Elixiriin viralliset dokumentit `Float`ista: https://hexdocs.pm/elixir/Float.html
- IEEE-standardi liukulukuaritmetiikalle (IEEE 754): https://ieeexplore.ieee.org/document/4610935
