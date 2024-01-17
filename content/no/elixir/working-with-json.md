---
title:                "Arbeide med json"
html_title:           "Elixir: Arbeide med json"
simple_title:         "Arbeide med json"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/working-with-json.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
JSON (JavaScript Object Notation) er en lettvekts og menneskelesbar måte å lagre og utveksle data på. Det er spesielt nyttig for webapplikasjoner som trenger å kommunisere med servere og utveksle data. Det er også et populært format for å lagre data for senere bruk.

# Slik gjør du det:
For å jobbe med JSON i Elixir, kan du bruke modulen `Jason`. Her er et eksempel på hvordan du kan konvertere data til JSON-format:

```
Elixir iex> data = %{name: "Jane", age: 30}
%Elixir.Map{age: 30, name: "Jane"}
Elixir iex> json = Jason.encode(data)
"{\"name\":\"Jane\",\"age\":30}"
```

Du kan også dekode JSON-data ved å bruke `Jason.decode` modulen:

```
Elixir iex> json = "{\"name\":\"Jane\",\"age\":30}"
"{\"name\":\"Jane\",\"age\":30}"
Elixir iex> data = Jason.decode(json)
%{age: 30, name: "Jane"}
```

# Dykk ned i det:
JSON ble utviklet på 1990-tallet som en enklere måte å utveksle data på. Det er et alternativ til XML-formatet som er mer komplisert og mindre lesbart. For å jobbe med JSON i Elixir, er `Jason` modulen den mest populære og anbefalte løsningen.

Andre muligheter for å jobbe med JSON i Elixir inkluderer `Poison` og `JSEX`, men disse modulene er vanligvis mindre stabile og mindre utfyllende enn `Jason`.

For å implementere `Jason`, bruker man en parser som er skrevet i Erlang, kalt `jsx`. Dette gir en rask og effektiv måte å håndtere store mengder JSON-data på.

# Se også:
Hvis du ønsker å lære mer om JSON i Elixir, kan du sjekke ut disse ressursene:

- Offisiell Elixir `Jason` dokumentasjon: https://hexdocs.pm/jason/readme.html
- Elixir Forum tråd om å arbeide med JSON: https://elixirforum.com/t/how-to-work-with-json-in-elixir/6208