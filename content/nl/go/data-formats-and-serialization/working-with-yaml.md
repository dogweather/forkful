---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:13:50.244135-07:00
description: "Werken met YAML in Go houdt in dat je YAML-bestanden (YAML Ain't Markup\
  \ Language), een gebruikersvriendelijke gegevensserialisatiestandaard, analyseert\u2026"
lastmod: 2024-02-19 22:05:09.393936
model: gpt-4-0125-preview
summary: "Werken met YAML in Go houdt in dat je YAML-bestanden (YAML Ain't Markup\
  \ Language), een gebruikersvriendelijke gegevensserialisatiestandaard, analyseert\u2026"
title: Werken met YAML
---

{{< edit_this_page >}}

## Wat & Waarom?

Werken met YAML in Go houdt in dat je YAML-bestanden (YAML Ain't Markup Language), een gebruikersvriendelijke gegevensserialisatiestandaard, analyseert naar Go-datastructuren en vice versa. Programmeurs doen dit om de eenvoud en leesbaarheid van YAML te benutten voor configuratiebestanden, applicatie-instellingen of gegevensuitwisseling tussen diensten en componenten geschreven in verschillende talen.

## Hoe:

Om met YAML in Go te werken, moet je eerst een bibliotheek importeren die ondersteuning biedt voor het parseren en serialiseren van YAML, aangezien de standaardbibliotheek van Go geen directe ondersteuning voor YAML omvat. De populairste bibliotheek hiervoor is "gopkg.in/yaml.v3". Hier is hoe je kunt beginnen:

1. **Het YAML-pakket installeren:**

```bash
go get gopkg.in/yaml.v3
```

2. **YAML parseren naar een Go struct:**

Definieer eerst een struct in Go die overeenkomt met de structuur van je YAML-gegevens.

```go
package main

import (
  "fmt"
  "gopkg.in/yaml.v3"
  "log"
)

type Config struct {
  Database struct {
    User     string `yaml:"user"`
    Password string `yaml:"password"`
  } `yaml:"database"`
}

func main() {
  var config Config
  data := `
database:
  user: admin
  password: secret
`
  err := yaml.Unmarshal([]byte(data), &config)
  if err != nil {
    log.Fatalf("error: %v", err)
  }
  fmt.Printf("Gebruiker: %s\nWachtwoord: %s\n", config.Database.User, config.Database.Password)
}
```

**Voorbeelduitvoer:**

```
Gebruiker: admin
Wachtwoord: secret
```

3. **Een Go struct naar YAML serialiseren:**

Zo converteer je een Go struct terug naar YAML.

```go
package main

import (
  "fmt"
  "gopkg.in/yaml.v3"
  "log"
)

func main() {
  config := Config{
    Database: struct {
      User     string `yaml:"user"`
      Password string `yaml:"password"`
    }{
      Gebruiker: "admin",
      Wachtwoord: "supergeheim",
    },
  }

  data, err := yaml.Marshal(&config)
  if err != nil {
    log.Fatalf("error: %v", err)
  }
  fmt.Printf("---\n%s\n", string(data))
}
```

**Voorbeelduitvoer:**

```yaml
---
database:
  user: admin
  password: supergeheim
```

## Diepgaand:

Het gebruik van YAML in softwareontwikkeling is gegroeid vanwege zijn leesbare formaat, waardoor het een ideale keuze is voor configuratiebestanden, documentatie of gegevensuitwisselingsformaten. In vergelijking met JSON, zijn tegenhanger, biedt YAML commentaren, scalaire typen en relatiefuncties, waardoor een rijker raamwerk voor gegevensserialisatie wordt geboden. Echter, zijn flexibiliteit en functies komen met de kosten van complexiteit in het parseren, wat kan leiden tot mogelijke veiligheidsrisico's als het niet zorgvuldig wordt behandeld (bijv., willekeurige code-uitvoering).

De bibliotheek "gopkg.in/yaml.v3" voor Go is een robuuste oplossing voor YAML-verwerking, die een balans vindt tussen gebruiksgemak en uitgebreide functieondersteuning. Vanaf de huidige staat, hoewel er alternatieven zijn zoals "go-yaml/yaml" (de bibliotheek achter "gopkg.in/yaml.v3"), hangt de gekozen versie meestal af van specifieke projectvereisten of persoonlijke voorkeur. Bij het omgaan met enorme datasets of prestatiekritieke applicaties, kunnen programmeurs simpelere formaten zoals JSON overwegen vanwege hun verminderde parseertijd en geheugenoverhead. Desondanks blijft YAML voor configuratiebestanden of instellingen waar menselijke leesbaarheid en gebruiksgemak van groot belang zijn, een sterke mededinger in het Go-ecosysteem.
