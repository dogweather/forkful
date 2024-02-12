---
title:                "Lavorare con YAML"
aliases:
- it/swift/working-with-yaml.md
date:                  2024-02-03T19:26:56.391731-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lavorare con YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa & Perché?
YAML, acronimo di "YAML Ain't Markup Language", è uno standard di serializzazione dati amichevole per l'utenza, adatto a tutti i linguaggi di programmazione. Gli sviluppatori lo utilizzano per file di configurazione, messaggistica tra processi e archiviazione dati poiché la sua leggibilità è molto più vicina all'inglese semplice rispetto ad altri formati di dati come XML o JSON, rendendolo più semplice da comprendere e scrivere.

## Come fare:
Swift non include il supporto integrato per l'analisi e la serializzazione di YAML, rendendo necessario l'uso di librerie di terze parti. Una scelta popolare è `Yams`, una libreria per lavorare con YAML in Swift.

Prima cosa, devi aggiungere `Yams` al tuo progetto. Se stai usando il Swift Package Manager, puoi aggiungerlo come dipendenza nel tuo file `Package.swift`:

```swift
dependencies: [
    .package(url: "https://github.com/jpsim/Yams.git", da: "4.0.0")
]
```

### Analizzare YAML in Swift
Ipotesi di avere la seguente configurazione YAML per una semplice app:

```yaml
name: MyApp
version: 1.0
environment: development
features:
  - login
  - notifications
```

Ecco come puoi analizzare questa stringa YAML in Swift usando `Yams`:

```swift
import Yams

let yamlString = """
name: MyApp
version: 1.0
environment: development
features:
  - login
  - notifications
"""

do {
    if let data = try Yams.load(yaml: yamlString) as? [String: Any] {
        print(data)
        // Esempio di accesso ai dati analizzati
        if let name = data["name"] as? String {
            print("Nome App: \(name)")
        }
    }
} catch {
    print("Errore nell'analisi di YAML: \(error)")
}
```

Output di esempio:

```
["name": MyApp, "version": 1.0, "environment": "development", "features": ["login", "notifications"]]
Nome App: MyApp
```

### Serializzare Oggetti Swift in YAML
Convertire un oggetto Swift in una stringa YAML è altrettanto semplice con `Yams`. Supponiamo di avere la stessa struttura dati che deve essere serializzata:

```swift
let appInfo = [
    "name": "MyApp",
    "version": 1.0,
    "environment": "development",
    "features": ["login", "notifications"]
] as [String : Any]

do {
    let yamlString = try Yams.dump(object: appInfo)
    print(yamlString)
} catch {
    print("Errore nella serializzazione in YAML: \(error)")
}
```

Questo produrrà una Stringa formattata in YAML:

```yaml
environment: development
features:
  - login
  - notifications
name: MyApp
version: 1.0
```

Questi esempi dimostrano le operazioni di base per lavorare con YAML nelle applicazioni Swift. Ricorda, sebbene YAML eccella in leggibilità umana e facilità d'uso, considera sempre le esigenze specifiche della tua applicazione, specialmente per quanto riguarda prestazioni e complessità, quando scegli il tuo formato di serializzazione dei dati.
