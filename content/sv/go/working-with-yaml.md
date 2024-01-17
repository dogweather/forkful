---
title:                "Arbeta med yaml"
html_title:           "Go: Arbeta med yaml"
simple_title:         "Arbeta med yaml"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/working-with-yaml.md"
---

{{< edit_this_page >}}

Vad och Varför?
YAML, vilket står för "YAML Ain't Markup Language", är ett sätt att strukturera och organisera data i en enkel, läsbar och programmerbar format. YAML används ofta av utvecklare för att hantera konfigurationsfiler, specifikationer och andra värdefulla data som behöver ordnas på ett lättförståeligt sätt.

Hur man gör:
Använd Go-paketet "gopkg.in/yaml.v2" för att arbeta med YAML i dina Go-projekt. Här är ett exempel på hur du kan läsa in en YAML-fil och hämta värden från den i Go:

```Go
package main

import (
  "fmt"
  "io/ioutil"
  "log"

  "gopkg.in/yaml.v2"
)

type Config struct {
  Database struct {
    Host     string `yaml:"host"`
    Port     int    `yaml:"port"`
    Username string `yaml:"username"`
    Password string `yaml:"password"`
    Database string `yaml:"database"`
  } `yaml:"database"`
}

func main() {
  file, err := ioutil.ReadFile("config.yaml")
  if err != nil {
    log.Fatal(err)
  }

  var config Config
  err = yaml.Unmarshal(file, &config)
  if err != nil {
    log.Fatal(err)
  }

  fmt.Printf("Databasvärdet är: %s:%d \n", config.Database.Host, config.Database.Port)
  fmt.Printf("Användarnamnet är: %s \n", config.Database.Username)
  fmt.Printf("Lösenordet är: %s \n", config.Database.Password)
  fmt.Printf("Databasnamnet är: %s \n", config.Database.Database)

}
```

Output: 
```
Databasvärde: localhost:3306 
Användarnamn: myusername 
Lösenord: mypassword 
Databasnamn: mydatabase 
```

Djupdykning:
YAML utvecklades ursprungligen av Ingy döt Net som ett enkelt och bättre alternativ till XML och JSON för att representera data i en hierarkisk form. Det är ett vanligt format inom DevOps-communityn för att hantera konfigurationsfiler och deployment-specifikationer. Det finns också alternativ för att arbeta med YAML i Go, som till exempel "kyleconroy/go-yaml" biblioteket. YAML stöder också kommentarer, vilket gör det mer läsbart och lättförståeligt.

Se även:
- Officiell YAML hemsida: https://yaml.org/
- Paketet "gopkg.in/yaml.v2" dokumentation: https://pkg.go.dev/gopkg.in/yaml.v2
- Alternativt YAML-paket för Go: https://pkg.go.dev/github.com/kyleconroy/go-yaml/yaml