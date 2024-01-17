---
title:                "Lavorare con YAML"
html_title:           "Swift: Lavorare con YAML"
simple_title:         "Lavorare con YAML"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/working-with-yaml.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?
Lavorare con YAML è una pratica comune tra i programmatori per organizzare e gestire un grande numero di dati strutturati in un formato facile da leggere e modificare. Questo formato utilizza uno schema di indentazione per rappresentare i dati in modo strutturato, rendendolo ideale per gestire dati complessi.

## Come Fare:
Ecco un esempio di come utilizzare YAML in Swift per creare un dizionario di dati:

```Swift
let data = """
name: John
age: 25
favorite_color: Blue
"""

if let dict = try? YAMLDecoder().decode(Dictionary<String, String>.self, from: data),
   let name = dict["name"],
   let age = dict["age"],
   let favoriteColor = dict["favorite_color"] {

   print("\(name) is \(age) years old and their favorite color is \(favoriteColor)")
}
```

Output:
```
John is 25 years old and their favorite color is Blue
```

## Approfondimento:
YAML (o "YAML Ain't Markup Language") è stato creato nel 2001 come un formato di serializzazione dei dati orientato al riuso e alla leggibilità. Oggi, viene ampiamente utilizzato in applicazioni web, configurazioni di server e nei file di configurazione di Ansible.

Come alternativa a YAML, esistono altri formati di serializzazione dei dati come JSON e XML. Tuttavia, YAML è molto più leggibile e facile da scrivere rispetto a questi formati.

Per lavorare con YAML in Swift, è possibile utilizzare la libreria di terze parti "Yams" o la libreria nativa "Codable". Entrambe consentono di codificare e decodificare dati YAML in oggetti Swift senza dover scrivere manualmente il codice per il parsing dei dati.

## Vedi anche:
- [YAML.org](https://yaml.org/)
- [Yams on GitHub](https://github.com/jpsim/Yams)
- [Codable on Apple Developer Documentation](https://developer.apple.com/documentation/swift/codable)