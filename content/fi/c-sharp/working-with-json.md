---
title:                "Työskentely jsonin kanssa"
html_title:           "C#: Työskentely jsonin kanssa"
simple_title:         "Työskentely jsonin kanssa"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/working-with-json.md"
---

{{< edit_this_page >}}

## Miksi

JSON (JavaScript Object Notation) on yksi yleisimmin käytetyistä tiedon muotoilutavoista ohjelmoinnissa, sillä se on helppolukuinen ja yhteensopiva useiden ohjelmointikielten kanssa. JSONia käytetään erityisesti siirtämään ja tallentamaan tietoja web-sovelluksissa ja rajapinnoissa.

## Miten

```C#
// Luodaan JSON-objekti C#-koodilla
string json = @"{
    'opiskelija': {
        'nimi': 'Matti Meikäläinen',
        'opiskelijanumero': 12345,
        'kurssit': ['Ohjelmoinnin perusteet', 'Tietokannat']
    }
}";

// Muunnetaan JSON-objekti C#-muotoon
var opiskelija = JObject.Parse(json);

// Tulostetaan opiskelijan nimi ja opiskelijanumero
Console.WriteLine("Nimi: " + opiskelija["nimi"]);
Console.WriteLine("Opiskelijanumero: " + opiskelija["opiskelijanumero"]);

// Tulostetaan kaikki opiskelijan kurssit
foreach (var kurssi in opiskelija["kurssit"])
{
    Console.WriteLine(kurssi);
}

// Tulostaa:
// Nimi: Matti Meikäläinen
// Opiskelijanumero: 12345
// Ohjelmoinnin perusteet
// Tietokannat
```

## Syvällinen perehtyminen

JSON-objektit koostuvat avain-arvo pareista, joissa avain on merkkijono ja arvo voi olla mikä tahansa JSONin sallima tietotyyppi, kuten merkkijono, numero tai lista. JSONia voi lukea ja muuntaa C#-kielen avulla käyttämällä esimerkiksi Newtonsoft.JSON-kirjastoa. JSON-tietoja voi myös muokata ja luoda C#-koodeilla.

See Also:

- [JSON.NET dokumentaatio](https://www.newtonsoft.com/json/help/html/Introduction.htm)
- [Microsoftin C#-opetusohjelma](https://docs.microsoft.com/fi-fi/dotnet/csharp/programming-guide/concepts/json/#creating-json-structures)