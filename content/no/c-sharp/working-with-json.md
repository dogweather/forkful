---
title:                "Arbeide med json"
html_title:           "C#: Arbeide med json"
simple_title:         "Arbeide med json"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/working-with-json.md"
---

{{< edit_this_page >}}

## Hvorfor

Å arbeide med JSON er viktig for å kunne lagre og utveksle data mellom forskjellige programmer. Det er en veldig populær format for datautveksling på tvers av plattformer og språk.

## Hvordan

For å kunne bruke JSON i C#, må du først importere en pakke som støtter dette formatet, som for eksempel Newtonsoft.Json. Deretter kan du bruke følgende kode for å konvertere en JSON-streng til et objekt:

```C#
string jsonString = @"{'navn':'Maria', 'alder': 25, 'hobbyer':['fotball', 'musikk']}";

Person person = JsonConvert.DeserializeObject<Person>(jsonString);
```

Du kan også konvertere et objekt til JSON-streng ved hjelp av følgende kode:

```C#
Person person = new Person
{
    Navn = "Maria",
    Alder = 25,
    Hobbyer = new List<string> { "fotball", "musikk" }
};

string jsonString = JsonConvert.SerializeObject(person);
```

Denne pakken gir deg også muligheten til å jobbe med JSON-filer. For eksempel kan du lese en JSON-fil og konvertere den til et objekt ved hjelp av følgende kode:

```C#
using (StreamReader sr = new StreamReader("person.json"))
{
    string jsonString = sr.ReadToEnd();

    Person person = JsonConvert.DeserializeObject<Person>(jsonString);
}
```

For mer informasjon om hvordan du bruker denne pakken, kan du ta en titt på dokumentasjonen: https://www.newtonsoft.com/json/help/html/Introduction.htm

## Dypdykk

JSON står for JavaScript Object Notation, og er en lett og leselig måte å lagre og utveksle data på. Det er basert på JavaScripts objektnotasjon og brukes mye i webutvikling. En av fordelene med JSON er at det er uavhengig av programmeringsspråk, noe som gjør det enkelt å utveksle data mellom forskjellige systemer.

I C# blir JSON vanligvis representert som en streng, og derfor trenger du en parser for å konvertere den til et objekt som kan brukes i koden din. Newtonsoft.Json er en av de mest populære parserne for JSON i C#.

Det er også viktig å merke seg at JSON-objekter støtter forskjellige datatyper som tekst, tall, boolske verdier og lister av objekter. Dette gjør det fleksibelt og egnet for å lagre strukturerte data.

## Se også

- Dokumentasjonen for Newtonsoft.Json: https://www.newtonsoft.com/json/help/html/Introduction.htm
- En tutorial om å jobbe med JSON i C#: https://www.c-sharpcorner.com/article/working-with-json-in-C-Sharp/ 
- En oversikt over de forskjellige datatypene som støttes av JSON: https://www.json.org/json-en.html