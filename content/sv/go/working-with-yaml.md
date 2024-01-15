---
title:                "Att arbeta med yaml."
html_title:           "Go: Att arbeta med yaml."
simple_title:         "Att arbeta med yaml."
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/working-with-yaml.md"
---

{{< edit_this_page >}}

## Varför

YAML är ett populärt dataformat som används för att konfigurera och strukturera information på ett lättläst sätt. Det är ett vanligt val när det gäller att lagra strukturerad data och har enkla syntaxregler som gör det lätt att arbeta med. Genom att lära dig Go kan du enkelt hantera YAML-formaterade data och integrera det i dina Go-applikationer.

## Hur man gör

För att kunna arbeta med YAML i Go behöver du först installera ett YAML-paket. Det finns flera paket att välja mellan, men för detta exempel kommer vi att använda "gopkg.in/yaml.v3". Börja med att importera paketet:

```Go
import (
  "fmt"
  "log"

  "gopkg.in/yaml.v3"
)
```

Nu kan vi skapa en struktur som matchar vår YAML-data. I detta exempel ska vi arbeta med en fil som innehåller information om en person:

```Go
type Person struct {
  Name string `yaml:"name"`
  Age int `yaml:"age"`
  Occupation string `yaml:"occupation"`
}
```

För att läsa in YAML-data från en fil och konvertera den till vår struktur, använder vi funktionen "decode" från YAML-paketet:

```Go
func main() {
  // Läs in YAML-data från en fil
  data, err := ioutil.ReadFile("person.yaml")
  if err != nil {
    log.Fatal(err)
  }

  // Konvertera YAML-data till vår struktur "Person"
  var person Person
  err = yaml.Unmarshal(data, &person)
  if err != nil {
    log.Fatal(err)
  }

  // Skriv ut personens information
  fmt.Printf("Namn: %s\nÅlder: %d\nYrke: %s\n", person.Name, person.Age, person.Occupation)
}
```

Om vi nu skriver ut filens innehåll får vi följande output:

```Go
Namn: Adam
Ålder: 35
Yrke: Utvecklare
```

För att skapa en YAML-fil utifrån vår struktur kan vi använda funktionen "encode":

```Go
// Skapa en ny instans av vår struktur
person := Person{
  Name: "Eva",
  Age: 28,
  Occupation: "Designer",
}

// Konvertera vår struktur till YAML
data, err = yaml.Marshal(person)
if err != nil {
  log.Fatal(err)
}

// Skapa en ny fil med vår YAML-data
err = ioutil.WriteFile("new_person.yaml", data, 0644)
if err != nil {
  log.Fatal(err)
}
```

Detta kommer att skapa en ny fil "new_person.yaml" med följande innehåll:

```yaml
name: Eva
age: 28
occupation: Designer
```

## Djupdykning

YAML har ett antal funktioner som gör det till ett praktiskt verktyg när det gäller att strukturera data. Här är några tips för att arbeta mer effektivt med YAML i Go:

- Använd taggar för att definiera namnet på dina variabler och göra koden mer läsbar. I exemplet tidigare använde vi taggen "yaml" för att definiera variablernas namn i vår struktur.
- Använd "omitempty" för att undvika att lägga till tomma värden i din YAML-fil. Om en variabel är tom kommer den inte att inkluderas i filen.
- Om du vill lägga till kommentarer i din YAML-fil kan du använda "#"-tecknet följt av din kommentar.
- För att hantera mer komplexa YAML-filer med inbäddade listor och dictionaries kan du använda "map[string]interface{}" för att dekodera och "map[interface{}]interface{}" för att koda.

## Se också

- [Dokumentation för Go YAML-paket](https://pkg.go.dev/gopkg.in/yaml.v3)
- [YAML-specifikationen](https://yaml.org/spec/1.2/spec.html)
- [YAML-guide på svenska](https://eemeli.org/yaml/)