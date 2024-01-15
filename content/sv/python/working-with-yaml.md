---
title:                "Att arbeta med YAML"
html_title:           "Python: Att arbeta med YAML"
simple_title:         "Att arbeta med YAML"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/working-with-yaml.md"
---

{{< edit_this_page >}}

## Varför

YAML (YAML Ain't Markup Language) är ett vanligt filformat som används för att strukturera och lagra data på ett läsbart och lättförståeligt sätt. Genom att läsa och skriva YAML-filer kan programmerare effektivt hantera och bearbeta data i sina program.

## Så här gör du

För att använda YAML i dina Python-program behöver du först importera "yaml" biblioteket. Detta kan göras genom att skriva följande kod:

```Python
import yaml
```

För att läsa en YAML-fil och lagra datan i en variabel kan du använda följande kod:

```Python
with open("data.yml") as fil:
    data = yaml.safe_load(fil)
```

Du kan nu komma åt datan i din YAML-fil genom att använda variabeln "data". Till exempel, om din YAML-fil innehåller en lista av användarnamn och lösenord, kan du komma åt den som följande:

```Python
users = data["användarlista"]
for user in users:
    print("Användarnamn: " + user["användarnamn"])
    print("Lösenord: " + user["lösenord"])
```

Du kan också skriva till en YAML-fil genom att använda "yaml.dump()" funktionen. Till exempel, om du vill spara en lista av användare i en YAML-fil, kan du göra det genom att skriva följande kod:

```Python
users = [
    {"användarnamn": "John", "lösenord": "asdf123"},
    {"användarnamn": "Jane", "lösenord": "qwerty456"}
]
with open("data.yml", "w") as fil:
    yaml.dump(users, fil)
```

## Djupdykning

Det finns många sätt att strukturera data i en YAML-fil. Nedan följer några viktiga aspekter att tänka på när du arbetar med YAML:

- Indentering används för att indikera inbäddade objekt och listor.
- Hash-tecken (#) används för att kommentera koden och påverkar inte koden i YAML-filen.
- Enkelcitat eller dubbelcitat kan användas för att omge en sträng, men det finns ingen skillnad i hur de tolkas av YAML.

Det är också värt att nämna att YAML är ett utvidgningsbart format, vilket betyder att du kan skapa dina egna datatyper och använda dem i YAML-filer. Detta kan vara särskilt användbart om du vill strukturera data på ett specifikt sätt för ditt program.

## Se även

- [YAML-språkspecifikation](https://yaml.org/spec/1.2/spec.html)
- [YAML på Python.org](https://pyyaml.org/)
- [YAML-tutorial från W3C](https://www.w3schools.com/python/python_yaml.asp)