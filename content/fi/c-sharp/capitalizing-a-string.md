---
title:                "Merkkijonon isojen kirjainten muuttaminen"
html_title:           "C#: Merkkijonon isojen kirjainten muuttaminen"
simple_title:         "Merkkijonon isojen kirjainten muuttaminen"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Pisteiden tai välilyöntien kanssa peukaloinnin sijaan ohjelmoijat voivat käyttää C# -kieltä päästäkseen yksinkertaisemmaksi tekemällä alustan voimisteltuja kielimuotoja, jotka ovat koko tunnoltaan päälaelleen ja kuvaa esimerkiksi, kuinka alkuun päästään. Tarkoituksena on parantaa koodin luettavuutta ja ymmärrettävyyttä.

## Miten:
Käytä seuraavaa koodinpätkää merkkijonojen kiertämiseen pää käsittelyyn C#:
```C#
string s = "esimerkki";
s = s.ToUpper();
Console.WriteLine(s); // Tulostaa "ESIMERKKI"
```

## Syväsukellus:
Mikäli haluatte tietää enemmän, merkkijonon suurilla kirjaimilla kirjoittaminen C#:ssa on ollut vaihtoehtoinen tapa vuosien ajan. Toivon että tämä artikkeli tulee toimimaan hyvänä aloituspaikkana, mikäli etsitte laajempaa ymmärrystä aiheesta.

## Katso myös:
* [Miksi koodarin on syytä kirjoittaa suurilla kirjaimilla] (https://www.google.com/)
* [C#:n virallinen dokumentaatio merkkijonon käsittelyyn] (https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/)