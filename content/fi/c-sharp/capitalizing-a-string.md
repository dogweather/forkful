---
title:                "Merkkijonon pääkirjaintaminen"
html_title:           "C#: Merkkijonon pääkirjaintaminen"
simple_title:         "Merkkijonon pääkirjaintaminen"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Stringin pääkirjainmuunnos tarkoittaa kielen tai ohjelmiston pääkirjainten vaihtoehtoista käyttöä. Ohjelmoijat tekevät niin esimerkiksi tekstin muotoilun tai uniikkitunnuksien luomisen vuoksi.

## Kuinka toimii:

Voit muuntaa C# stringin pääkirjaimiksi käyttäen ToUpper()-metodia. Esimerkkikoodi:

```C#
string muuttuja = "moi, maailma!";
muuttuja = muuttuja.ToUpper();
Console.WriteLine(muuttuja);  
```

Tämän tulokset:

```C#
"MOI, MAAILMA!"
```

## Syvempi sukellus:

Stringin pääkirjainmuunnos on ollut osa ohjelmointikieliä jo pitkään, ja toisin kuin C#, jotkut kielet (kuten JavaScript) sisältävät string-metodin, joka muuntaa vain stringin ensimmäisen kirjaimen isoksi.

C#-ssa on myös muita tapoja tehdä sama. Esimerkiksi, voit käyttää Culture- tai TextInfo-luokkaa. Tässä on esimerkki, jossa käytetään TextInfoa:

```C#
CultureInfo cultureInfo = CultureInfo.CurrentCulture;
TextInfo textInfo = cultureInfo.TextInfo;
string muuttuja = "moi, maailma!";
muuttuja = textInfo.ToTitleCase(muuttuja);
Console.WriteLine(muuttuja);  
```

Huomaa, että ToTitleCase()-metodi päivittää vain stringin ensimmäisen kirjaimen, joten se saattaa olla hyvä vaihtoehto tietyissä tapauksissa.

## Katso myös:

- [Microsoftin dokumentaatio ToUpper()-metodista](https://docs.microsoft.com/en-us/dotnet/api/system.string.toupper)
- [Microsoftin dokumentaatio ToTitleCase()-metodista](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.textinfo.totitlecase)