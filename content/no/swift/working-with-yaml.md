---
title:                "Arbeid med YAML"
date:                  2024-01-19
simple_title:         "Arbeid med YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/working-with-yaml.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
YAML er et dataformat for å strukturere data, lik JSON og XML. Programmerere bruker YAML fordi det er lettleselig og glimrende for konfigurasjonsfiler og datautveksling.

## Slik gjør du:
Swift har ingen innebygd YAML-støtte, så du må bruke et tredjepartsbibliotek, som `Yams`. Her er hvordan du leser YAML:

```Swift
import Yams

let yamlString = """
name: Ole
occupation: Programmer
skills:
  - Swift
  - Git
  - Linux
"""

do {
    if let person = try Yams.load(yaml: yamlString) as? [String: Any] {
        print(person["name"] ?? "Ingen navn")
    }
} catch {
    print("Feil ved parsing av YAML.")
}
```

Sample output:
```
Ole
```

For å skrive YAML i Swift:

```Swift
import Yams

let person = [
    "name": "Kari",
    "occupation": "Developer",
    "skills": ["Swift", "Docker", "Kubernetes"]
]

do {
    let yamlString = try Yams.dump(object: person)
    print(yamlString)
} catch {
    print("Kunne ikke skrive YAML.")
}
```

Sample output:
```
name: Kari
occupation: Developer
skills:
  - Swift
  - Docker
  - Kubernetes
```

## Dykk dypere
YAML startet i 2001 som et enklere alternativ til XML. Det står for "YAML Ain't Markup Language". Det er ofte brukt med Docker, Kubernetes og i mange moderne utviklingsmiljøer. YAML er lett å feil med på grunn av dens kritiske bruk av innrykk, noe JSON og XML ikke har. Alternativer inkluderer JSON for datautveksling og .env-filer for enkle konfigurasjoner.

## Se også
- YAML spesifikasjon: https://yaml.org/spec/1.2/spec.html
- `Yams` GitHub-repo: https://github.com/jpsim/Yams
- Swift Package Manager dokumentasjon: https://swift.org/package-manager/
