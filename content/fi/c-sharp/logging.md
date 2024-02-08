---
title:                "Lokitus"
date:                  2024-01-26T01:01:01.388589-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lokitus"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/logging.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Lokitus on sovelluksen tapahtumien ja tiedon tulostuksen tallentamista suoritusaikana. Ohjelmoijat luovat lokeja vikojen diagnostiikkaa, ohjelmiston suorituskyvyn valvontaa, käyttäjän toimien seurantaa ja turvallisuus- sekä liiketoimintastandardien noudattamisen ylläpitoa varten.

## Miten:
C#:ssa voi käyttää sisäänrakennettua `System.Diagnostics` nimiavaruutta tai kolmannen osapuolen kirjastoja kuten NLog tai log4net. Tässä on nopea esimerkki `.NET Core`:n saatavilla olevaa `ILogger`-rajapintaa käyttäen:

```C#
using Microsoft.Extensions.Logging;
using System;

public class Program
{
    public static void Main()
    {
        using var loggerFactory = LoggerFactory.Create(builder => {
            builder.AddConsole();
        });

        ILogger logger = loggerFactory.CreateLogger<Program>();

        logger.LogInformation("Tämä on informatiivinen viesti.");
        logger.LogWarning("Tämä on varoitusviesti.");
        logger.LogError("Tämä on virheviesti.");
    }
}
```

Esimerkkilokitus:
```
info: Program[0]
      Tämä on informatiivinen viesti.
warn: Program[0]
      Tämä on varoitusviesti.
fail: Program[0]
      Tämä on virheviesti.
```

## Syväsukellus
Lokituksen historia ohjelmistokehityksessä on lähes yhtä vanha kuin itse ohjelmointi; se on kehittynyt yksinkertaisista tulostuslauseista monimutkaisiin, mukautettaviin järjestelmiin. Alun perin lokitusta tehtiin kirjoittamalla tiedostoihin tai konsoliin, mutta tämä on kasvanut laajemmiksi rakenteiksi, kuten lokien aggregointijärjestelmiä ja hajautettuja jäljitysalustoja (kuten ELK-pinon tai Jaeger).

Vaihtoehtoja `.NET`:n sisäänrakennetulle lokitukselle ovat kolmannen osapuolen kirjastot:
- **NLog**: monipuolinen ja helppo asentaa, sisältäen paljon ominaisuuksia lokien reititykseen, muotoiluun ja suodattamiseen.
- **log4net**: Java log4j-kirjaston innoittama, se on erittäin mukautettavissa XML:n kautta ja tukee monenlaisia lokitietovarastoja.

Kun tulee kyse toteutuksen yksityiskohdista, lokitusabstraktiosi (kuten Microsoft.Extensions.Logging) ja taustalla olevan lokituspalveluntarjoajan valinta voivat merkittävästi vaikuttaa sovelluksesi suorituskykyyn ja luotettavuuteen. On tärkeää määritellä lokitasot asianmukaisesti ja varmistaa, ettei lokien kirjoitus muodostu pullonkaulaksi.

Lisäksi, rakennoitua lokitusta - jossa logit eivät ole vain merkkijonoja vaan avain-arvo -pareja tai objekteja - mahdollistaa tarkemmat ja toimenpiteisiin johtavat lokit, jotka ovat helpompi kysellä ja analysoida.

## Katso myös
- [Microsoft.Extensions.Logging Dokumentaatio](https://docs.microsoft.com/en-us/aspnet/core/fundamentals/logging/)
- [NLog Dokumentaatio](https://nlog-project.org/documentation/)
- [log4net Dokumentaatio](https://logging.apache.org/log4net/)
- [Serilog Dokumentaatio](https://serilog.net/) (esimerkkinä rakennoitetusta lokituksesta)
