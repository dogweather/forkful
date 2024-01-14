---
title:                "Swift: Lavorare con Yaml"
simple_title:         "Lavorare con Yaml"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/working-with-yaml.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore Swift alla ricerca di un modo per gestire i tuoi dati in maniera efficace e leggibile, allora dovresti considerare di lavorare con YAML. Questo linguaggio di markup ti consente di organizzare i tuoi dati in un formato facile da leggere e da modificare. Nella sezione successiva, esploreremo insieme come utilizzare YAML nella tua programmazione Swift.

## Come

```Swift
import YAML

let myDictionary = [
    "nome": "Maria",
    "età": 28,
    "hobby": ["cucinare", "leggere", "correre"],
    "animali_domestici": true
]

let yamlString = try YAMLEncoder().encode(myDictionary)
print(yamlString)

// Output:
// nome: Maria
// età: 28
// hobby:
//     - cucinare
//     - leggere
//     - correre
// animali_domestici: true
```

Come puoi vedere dall'esempio sopra, utilizzare YAML in Swift è molto semplice. Iniziamo importando il pacchetto YAML e creando un dizionario con alcune informazioni. Poi, utilizziamo YAMLEncoder per codificare il nostro dizionario in una stringa YAML e infine stampiamo l'output. Nota come la sintassi di YAML sia molto facile da leggere e da interpretare.

## Approfondimento

Oltre alla semplice codifica e decodifica di dati con YAML, è possibile effettuare diverse operazioni avanzate come l'aggiunta di commenti, la gestione dei tipi di dati e la creazione di strutture più complesse come array e nested dictionaries. Per saperne di più su queste funzionalità, puoi consultare la documentazione ufficiale di YAML per Swift.

## Vedi anche

- [Documentazione ufficiale di YAML per Swift](https://github.com/behrang/YamlSwift)
- [Tutorial su come utilizzare YAML con Swift](https://www.raywenderlich.com/1754836-yaml-tutorial-how-to-parse-yaml-rome)
- [Esempi di codice su GitHub con l'utilizzo di YAML in Swift](https://github.com/search?q=yaml+swift)