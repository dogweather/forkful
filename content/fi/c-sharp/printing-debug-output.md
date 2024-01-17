---
title:                "Virheenjäljitystulostuksen tulostaminen"
html_title:           "C#: Virheenjäljitystulostuksen tulostaminen"
simple_title:         "Virheenjäljitystulostuksen tulostaminen"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Painetun debug-tulosteen avulla kehittäjät voivat seurata koodin toimintaa ja etsiä mahdollisia virheitä. Se on tärkeä työkalu ohjelman toimivuuden varmistamiseksi ja tehokkaan vianetsinnän tekemiseksi.

## Kuinka:
```C#
Console.WriteLine("Tämä on debug-tulosteen esimerkki");

// Output: Tämä on debug-tulosteen esimerkki
```

Voit myös tallentaa debug-tulosteen tiedostoon käyttämällä ```Debug.Write``` tai ```Debug.WriteLine``` komentoja. Tämä helpottaa virheiden jäljittämistä pidemmissä ohjelmissa.

## Syväsukellus:
Debug-tulosteiden käyttö on yleinen käytäntö ohjelmoinnissa jo vuosien ajan. Se auttaa kehittäjiä nopeasti löytämään ja korjaamaan ongelmia koodissaan. Toiset ohjelmointikielet, kuten Python ja Java, käyttävät myös vastaavia debug-tulostetoimintoja.

Joskus debug-tulosteiden käyttö voi hidastaa ohjelman suorituskykyä, joten se kannattaa poistaa lopullisesta tuotantoversiosta.

## Katso myös:
- [Microsoftin virallinen dokumentaatio debug-tulosteista C# -ohjelmoinnissa](https://docs.microsoft.com/fi-fi/dotnet/api/system.diagnostics.debug?view=netcore-3.1)
- [Vinkkejä debug-tulosteiden käytöstä Visual Studion kanssa](https://docs.microsoft.com/fi-fi/visualstudio/debugger/how-to-use-the-debugger-and-troubleshoot?view=vs-2019)