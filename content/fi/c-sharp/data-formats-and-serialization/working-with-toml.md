---
date: 2024-01-26 04:20:09.444108-07:00
description: "TOML on lyhenne sanoista Tom's Obvious, Minimal Language, eli selke\xE4\
  \ ja minimalistinen konfiguraatiotiedostomuoto, joka on helppo lukea sen selke\xE4\
  n\u2026"
lastmod: '2024-03-13T22:44:56.597544-06:00'
model: gpt-4-0125-preview
summary: "TOML on lyhenne sanoista Tom's Obvious, Minimal Language, eli selke\xE4\
  \ ja minimalistinen konfiguraatiotiedostomuoto, joka on helppo lukea sen selke\xE4\
  n semantiikan ansiosta."
title: "Ty\xF6skentely TOML:n kanssa"
weight: 39
---

## Mikä & Miksi?
TOML on lyhenne sanoista Tom's Obvious, Minimal Language, eli selkeä ja minimalistinen konfiguraatiotiedostomuoto, joka on helppo lukea sen selkeän semantiikan ansiosta. Ohjelmoijat käyttävät sitä konfiguraatiotiedostoissa, yksinkertaistaakseen tietojenvaihtoa järjestelmien välillä, sekä siksi, että se löytää tasapainon ihmisen luettavuuden ja koneen jäsentämisen välillä.

## Kuinka:
Asenna ensin TOML-jäsentäjä kuten `Tomlyn`. Käytä paketinhallintaasi:

```csharp
dotnet add package Tomlyn
```

Seuraavaksi, jäsenne TOML-tiedosto:

```csharp
using Tomlyn;
using Tomlyn.Model;
using System;

var tomlContent = @"
[owner]
name = 'Tom Preston-Werner'
dob = 1979-05-27T07:32:00Z";

var tomlTable = Toml.Parse(tomlContent).ToModel();

Console.WriteLine($"Omistaja: {tomlTable["owner"]["name"]}");
// Tuloste:
// Omistaja: Tom Preston-Werner
```

Nyt, luo ja kirjoita TOML:

```csharp
using Tomlyn;
using Tomlyn.Syntax;
using System;
using System.IO;

var doc = new DocumentSyntax
{
    Tables =
    {
        new TableSyntax("owner")
        {
            Items =
            {
                { "name", "Tom Preston-Werner" },
                { "dob", "1979-05-27T07:32:00Z" }
            }
        }
    }
};

var tomlString = doc.ToString();
File.WriteAllText("config.toml", tomlString);
Console.WriteLine("TOML kirjoitettu tiedostoon config.toml");
// Tuloste:
// TOML kirjoitettu tiedostoon config.toml
```

## Syväsukellus:
TOML:n loi Tom Preston-Werner, GitHubin perustajajäsen, vuonna 2013 vastauksena olemassa olevien formaattien kuten YAML:n ja JSON:n rajoituksiin konfiguraatioasetuksissa. Se on suunniteltu nimenomaan konfiguraatioita varten painottaen suoraviivaista ja yksiselitteistä lähestymistapaa.

Vaihtoehtoisia konfiguraatioformaatteja ovat YAML, JSON ja XML. Kuitenkin TOML erottuu olemalla ihmisläheisempi, etenkin konfiguraatiotiedostoissa, joita yleisesti muokataan käsin. JSON, vaikka kaikkialla läsnä oleva, on vähemmän luettava monimutkaisissa konfiguraatioissa, ja XML on sanallinen. YAML, vaikka samankaltainen luettavuudessa, voi muuttua monimutkaiseksi runsaan välilyöntien käytön ja tietyn sisällön turvallisuusriskien vuoksi.

Toteutuksen kannalta TOML keskittyy mappaukseen puhtaasti hajautustauluun, tehden tietojen poiminnasta ennustettavaa. Version 1.0.0 julkaisun myötä TOML vakiinnutti spesifikaationsa, parantaen vakautta ja työkalutukea.

## Katso Myös:
- Virallinen TOML GitHub-repo & Spesifikaatio: [github.com/toml-lang/toml](https://github.com/toml-lang/toml)
- Tomlyn, .NET-kirjasto: [github.com/xoofx/Tomlyn](https://github.com/xoofx/Tomlyn)
