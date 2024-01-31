---
title:                "Praca z JSON"
date:                  2024-01-19
simple_title:         "Praca z JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/working-with-json.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
JSON to format wymiany danych, łatwy do czytania dla człowieka i maszyny. Programiści używają JSON do przechowywania ustawień, komunikacji z API i serializacji obiektów w aplikacjach sieciowych.

## Jak zrobić:
Aby pracować z JSON w C#, używamy biblioteki `System.Text.Json`, która jest wbudowana od .NET Core 3.0. Poniżej znajdziesz przykłady serializacji i deserializacji.

Serializacja:
```C#
using System.Text.Json;

var obiekt = new { Imie = "Jan", Nazwisko = "Kowalski", Wiek = 30 };
string json = JsonSerializer.Serialize(obiekt);
Console.WriteLine(json);
```
Wyjście:
```
{"Imie":"Jan","Nazwisko":"Kowalski","Wiek":30}
```

Deserializacja:
```C#
using System.Text.Json;

string json = "{\"Imie\":\"Jan\",\"Nazwisko\":\"Kowalski\",\"Wiek\":30}";
var obiekt = JsonSerializer.Deserialize<dynamic>(json);
Console.WriteLine(obiekt.Imie);
```
Wyjście:
```
Jan
```

## Głębsze spojrzenie
JSON, czyli JavaScript Object Notation, narodził się jako część języka JavaScript, ale szybko zyskał niezależność jako standard. Alternatywami dla JSON są XML i YAML, ale żaden nie jest tak prosty. W C#, przed `System.Text.Json`, popularna była biblioteka `Newtonsoft.Json`, znana też jako Json.NET.

## Zobacz również
- Dokumentacja System.Text.Json: https://docs.microsoft.com/en-us/dotnet/api/system.text.json?view=net-5.0
- Strona główna Json.NET: https://www.newtonsoft.com/json
- Tutorial json.org: http://json.org/
