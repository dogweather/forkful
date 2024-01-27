---
title:                "Lavorare con YAML"
date:                  2024-01-19
html_title:           "Bash: Lavorare con YAML"
simple_title:         "Lavorare con YAML"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML è un formato human-friendly per dati. Programmatori lo usano per configurazione, facilitando la lettura e scrittura rispetto a JSON o XML.

## How to:
Ecco un esempio in Swift utilizzando la libreria `Yams` per leggere un file YAML.

```Swift
import Yams

let yamlStr = """
name: Mario Rossi
age: 35
languages:
  - Swift
  - Python
"""

do {
    if let data = try Yams.load(yaml: yamlStr) as? [String: Any] {
        print(data["name"] as? String ?? "N/A")  // Output: Mario Rossi
        print(data["age"] as? Int ?? 0)          // Output: 35
    }
} catch {
    print("Errore durante il parsing del YAML: \(error)")
}
```

Questo codice legge una stringa YAML e stampa i valori di `name` e `age`. Per usare `Yams`, installalo tramite Swift Package Manager.

## Deep Dive
YAML (“YAML Ain't Markup Language”) è nato nei primi anni 2000 come alternativa a XML per la configurazione e trasmissione dati. Altri formati includono JSON e TOML. YAML spicca per la sua leggibilità e viene spesso usato in progetti come Docker e Kubernetes. Internamente, libreria `Yams` in Swift trasforma il testo YAML in una struttura di dati utilizzabile nella programmazione.

## See Also
- La documentazione ufficiale di YAML: https://yaml.org
- Libreria `Yams` su GitHub: https://github.com/jpsim/Yams
- Swift Package Manager: https://swift.org/package-manager/
