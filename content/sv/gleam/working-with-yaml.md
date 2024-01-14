---
title:                "Gleam: Att arbeta med yaml"
simple_title:         "Att arbeta med yaml"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/working-with-yaml.md"
---

{{< edit_this_page >}}

## Varför

Att jobba med YAML kan vara en användbar färdighet för många programmerare. Med Gleam, en funktionell programmeringsspråk inspirerat av Erlang, kan du effektivt arbeta med YAML för att hantera konfigurationsfiler och datastrukturer.

## Hur man gör det

För att använda YAML i Gleam, behöver du först installera biblioteket `gleam-yaml` genom att lägga till det till din `gleam.toml` fil. Sedan kan du importera `Yaml` biblioteket i din kod och använda dess funktioner för att läsa och skriva YAML filer.

För att läsa en YAML fil, kan du använda `Yaml.read_file` funktionen och ange sökvägen till filen som ett argument. Resultatet kommer att vara ett `Result` värde som antingen är `Ok` med den lästa datan eller `Error` om något gick fel.

```
Gleam Yaml.read_file("config.yaml")
```

För att skriva en YAML fil, kan du använda `Yaml.write_file` funktionen och ange datan som ska skrivas samt sökvägen till den nya filen som argument. Om allt går väl, kommer en YAML fil att skapas baserat på den data som du tillhandahöll.

```
Gleam Yaml.write_file(my_data, "new_config.yaml")
```

## Deep Dive

När du arbetar med YAML i Gleam, är det viktigt att förstå hur datan är strukturerad. YAML-dokument består av nycklar och värden, separerade med kolon. Nycklar är alltid strängar och värden kan antingen vara en sträng, en siffra eller en lista.

Ett exempel på en YAML-dokument:

```
username: john
age: 35
hobbies:
  - surfing
  - fotografering
```

I Gleam kommer datan från YAML filen att konverteras till en `Map` datastruktur, där nycklarna är `String` värden och värdena kan vara valfri typ. För att få tillgång till dessa värden kan du använda `Map.get` funktionen och ange nyckeln som ett argument.

```
let user = Gleam Yaml.read_file("user.yaml")
case user {
  Ok(data) -> {
    Gleam IO.println(Map.get("username", data))
    // Output: john
    let hobbies = Map.get("hobbies", data)
    Gleam IO.println(List.to_string(hobbies))
    // Output: "surfing, fotografering"
  }
  Error(_) -> Gleam IO.println("Ett fel uppstod vid läsning av filen")
}
```

## Se också

- [Dokumentation för Yaml biblioteket](https://github.com/gleam-lang/gleam-yaml)
- [YAML officiell hemsida](https://yaml.org/)
- [Gleam officiell hemsida](https://gleam.run/)