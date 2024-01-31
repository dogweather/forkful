---
title:                "Werken met YAML"
date:                  2024-01-28T22:11:43.169953-07:00
model:                 gpt-4-0125-preview
simple_title:         "Werken met YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/go/working-with-yaml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Werken met YAML betekent het parseren en genereren van gegevens in het YAML-formaat, een voor mensen leesbare standaard voor gegevensserialisatie. Programmeurs doen dit om configuratiebestanden te beheren, gegevensuitwisseling tussen talen mogelijk te maken en complexe datastructuren te structureren.

## Hoe te:
Om met YAML in Go te werken, heb je een bibliotheek zoals `gopkg.in/yaml.v3` nodig. Installeer het met:

```bash
go get gopkg.in/yaml.v3
```

Zo parseer je YAML:

```Go
package main

import (
	"fmt"
	"log"
	"gopkg.in/yaml.v3"
)

var data = `
a: Makkelijk!
b:
  c: 2
  d: [3, 4]
`

type StructA struct {
	A string
	B StructB
}

type StructB struct {
	C int
	D []int
}

func main() {
	var s StructA

	err := yaml.Unmarshal([]byte(data), &s)
	if err != nil {
		log.Fatalf("fout: %v", err)
	}
	fmt.Println(s)
}
```

Uitvoer:

```
{Makkelijk! {2 [3 4]}}
```

YAML genereren:

```Go
package main

import (
	"fmt"
	"gopkg.in/yaml.v3"
)

func main() {
	data := StructA{
		A: "Makkelijk!",
		B: StructB{
			C: 2,
			D: []int{3, 4},
		},
	}

	d, err := yaml.Marshal(&data)
	if err != nil {
		log.Fatalf("fout: %v", err)
	}
	fmt.Printf("---\n%s\n", string(d))
}
```

Uitvoer:

```
---
a: Makkelijk!
b:
  c: 2
  d:
  - 3
  - 4
```

## Diepere duik
YAML is gestart in 2001, met het doel een mensvriendelijk gegevensuitwisselingsformaat te zijn. Het wordt gebruikt als een alternatief voor JSON en XML omdat het leesbaarder is en complexe datastructuren kan vertegenwoordigen. Go heeft geen ingebouwde ondersteuning voor YAML, daarom zijn bibliotheken van derden zoals `gopkg.in/yaml.v3` populair. De bibliotheek verpakt libyaml, een C YAML-parser en -emitter, voor efficiëntie en naleving van YAML-standaarden.

## Zie ook
- De YAML v3 pakketdocumentatie: https://pkg.go.dev/gopkg.in/yaml.v3
- Officiële YAML-website: https://yaml.org
- YAML-specificatie: https://yaml.org/spec/1.2/spec.html
- JSON naar YAML online converter: https://www.json2yaml.com/
