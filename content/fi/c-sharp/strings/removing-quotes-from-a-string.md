---
date: 2024-01-26 03:38:27.288244-07:00
description: "Merkkijonosta lainausmerkkien poistaminen C#:ss\xE4 tarkoittaa sit\xE4\
  , ett\xE4 poistat teksti\xE4si ymp\xE4r\xF6iv\xE4t \xE4rsytt\xE4v\xE4t kaksois-\
  \ (`\"`) tai yksinkertaiset (`'`)\u2026"
lastmod: '2024-03-13T22:44:56.561336-06:00'
model: gpt-4-0125-preview
summary: "Merkkijonosta lainausmerkkien poistaminen C#:ss\xE4 tarkoittaa sit\xE4,\
  \ ett\xE4 poistat teksti\xE4si ymp\xE4r\xF6iv\xE4t \xE4rsytt\xE4v\xE4t kaksois-\
  \ (`\"`) tai yksinkertaiset (`'`)\u2026"
title: Merkkijonosta lainausmerkkien poistaminen
---

{{< edit_this_page >}}

## Mitä & Miksi?
Merkkijonosta lainausmerkkien poistaminen C#:ssä tarkoittaa sitä, että poistat tekstiäsi ympäröivät ärsyttävät kaksois- (`"`) tai yksinkertaiset (`'`) lainausmerkit. Ohjelmoijat tekevät tämän puhdistaakseen dataa, valmistellakseen tietokantamerkintää varten tai tehdäkseen merkkijonoista turvallisia jatkokäsittelyä varten, jotta asiat eivät mene sekaisin, kun eksynyt lainausmerkki ilmaantuu.

## Miten:
```csharp
string withQuotes = "\"Hello, World!\"";
Console.WriteLine($"Alkuperäinen: {withQuotes}");

// Poista kaksoislainausmerkit
string withoutDoubleQuotes = withQuotes.Replace("\"", "");
Console.WriteLine($"Ilman kaksoislainausmerkkejä: {withoutDoubleQuotes}");

// Poista yksittäiset lainausmerkit (olettaen, että merkkijonossasi oli alun perin niitä)
string withSingleQuotes = "'Hello, World!'";
string withoutSingleQuotes = withSingleQuotes.Replace("'", "");
Console.WriteLine($"Ilman yksittäisiä lainausmerkkejä: {withoutSingleQuotes}");
```

Tuloste:
```
Alkuperäinen: "Hello, World!"
Ilman kaksoislainausmerkkejä: Hello, World!
Ilman yksittäisiä lainausmerkkejä: Hello, World!
```

## Syväsukellus
Lainausmerkkien poistamisen konsepti ei ole uusi tai erityisen monimutkainen, mutta se on kriittinen, koska lainausmerkkejä käytetään usein merkkijonojen rajaamiseen. Kun koodilohkoon tai datatiedostoon sisällytetään merkkijono, jossa on käsittelemättömiä lainausmerkkejä, se saattaa päättää merkkijonon ennenaikaisesti, mikä aiheuttaa virheitä tai turvallisuusongelmia, kuten injektiohyökkäyksiä.

Historiallisesti lainausmerkkien käsittely on ollut osa datan käsittelyn validointi- ja puhdistusprosessia. Vaikka `.Replace()`-metodi on suoraviivainen tapa poistaa lainausmerkkejä yksinkertaisesta merkkijonosta, monimutkaisemmissa tapauksissa, kuten sisäkkäisissä lainausmerkeissä tai ehdollisessa poistossa, saatat tarvita kehittyneempiä tekniikoita, kuten säännöllisiä lausekkeita.

Vaihtoehtoja `.Replace()`-metodille ovat `Regex`-luokan metodit, kun tarvitset hienosäätöä tai käsittelet malleja kiinteiden merkkien sijaan. Esimerkiksi `Regex.Unescape()` saattaa olla hyödyllinen käsiteltäessä escapattuja merkkejä.

Toteutuksen kannalta muista, että merkkijonot C#:ssa ovat muuttumattomia, mikä tarkoittaa, että joka kerta kun käytät `.Replace()`, luodaan uusi merkkijono. Tämä ei ole suuri ongelma pienille tai kertaluontoisille toiminnoille, mutta se on jotain, mitä kannattaa pitää mielessä suorituskyvyn kannalta suurten tai useiden merkkijonojen kohdalla.

## Katso myös:
- [String.Replace metodin dokumentaatio](https://docs.microsoft.com/en-us/dotnet/api/system.string.replace?view=netframework-4.8)
- [Säännölliset lausekkeet .NET:ssä](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expressions)
- [Turvallisen merkkijonon käsittelyn parhaat käytännöt](https://www.owasp.org/index.php/Data_Validation)
