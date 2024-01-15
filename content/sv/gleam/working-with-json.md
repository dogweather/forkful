---
title:                "Arbeta med json"
html_title:           "Gleam: Arbeta med json"
simple_title:         "Arbeta med json"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/working-with-json.md"
---

{{< edit_this_page >}}

## Varför

Att arbeta med JSON kan vara väldigt användbart eftersom det är ett vanligt format för datautbyte mellan olika applikationer. Oavsett om du bygger en webbapplikation eller utvecklar en mobilapp, kommer du sannolikt att behöva hantera JSON-data på ett eller annat sätt.

## Så här gör du

För att börja arbeta med JSON i Gleam, använd `gleam/json` paketet. För att installera paketet, öppna en terminal och kör följande kommando:

```
gleam install gleam/json
```

När paketet är installerat kan du importera det i din kod genom att lägga till följande längst upp i din `.gleam` fil:

```
import gleam/json
```

För att läsa in JSON-data i en variabel kan du använda funktionen `gleam/json.decode` och ange JSON-strängen och den förväntade typen av data som argument. Till exempel, om du har en JSON-sträng som innehåller en lista med användare, kan du läsa in den i en `User`-lista enligt följande:

```
let json_string = "{ "users": [ { "name": "Lisa", "age": 25 }, { "name": "Peter", "age": 30 } ] }"

let users = json.decode(json_string, &[User])
```

För att konvertera en Gleam-struktur till en JSON-sträng, kan du använda funktionen `gleam/json.encode` och ange strukturen som argument. Till exempel, om du har en `User`-struktur som du vill skicka som JSON-data, kan du göra det enligt följande:

```
let user = User("John", 35)

let json_string = json.encode(user)
```

## Djupdykning

När du arbetar med JSON i Gleam, är det viktigt att förstå att JSON-strängar måste följa ett visst format för att kunna läsas in korrekt. Till exempel måste alla nycklar vara omgivna av citattecken och alla värden måste vara antingen ett nummer, en sträng eller en boolean. Om din JSON-sträng inte följer detta format kan den inte läsas in korrekt.

En annan viktig sak att komma ihåg är att du måste ange den förväntade typen av data när du läser in JSON. Om typen inte matchar den faktiska datan i JSON-strängen kommer du att få ett felmeddelande.

## Se även

- [Official Gleam Documentation](https://gleam.run/documentation/)
- [Gleam JSON Package Documentation](https://hexdocs.pm/gleam_json/readme.html)