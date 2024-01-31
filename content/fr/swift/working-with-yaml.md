---
title:                "Travailler avec YAML"
date:                  2024-01-19
html_title:           "Bash: Travailler avec YAML"
simple_title:         "Travailler avec YAML"

category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML, "YAML Ain't Markup Language," est un format de sérialisation de données lisible par l'homme. Les développeurs l'utilisent pour configurer des projets, stocker des données et faciliter la communication entre différents services.

## How to:
Pour manipuler du YAML en Swift, on utilise souvent une bibliothèque tierce comme `Yams`. Vous devez ajouter Yams à votre projet via Swift Package Manager ou CocoaPods. Voici comment lire et écrire du YAML :

```Swift
import Yams

let yaml = """
- name: Harry Potter
  job: Wizard
- name: Hermione Granger
  job: Witch
"""

do {
    // Lecture
    if let people = try Yams.load(yaml: yaml) as? [[String: String]] {
        for person in people {
            print("\(person["name"] ?? "") est un(e) \(person["job"] ?? "").")
        }
    }

    // Écriture
    let newPerson = ["name": "Ron Weasley", "job": "Wizard"]
    let newYaml = try Yams.dump(object: newPerson)
    print(newYaml)
} catch {
    print("Erreur de traitement YAML: \(error)")
}
```

Résultat :

```
Harry Potter est un(e) Wizard.
Hermione Granger est un(e) Witch.
- job: Wizard
  name: Ron Weasley
```

## Deep Dive
YAML a été développé en 2001 par Clark Evans, Ingy döt Net et Oren Ben-Kiki. C'est souvent une alternative à JSON ou XML car c'est plus facile à lire et à écrire pour les êtres humains. Toutefois, attention, YAML peut être sujet aux erreurs dues à sa nature sensible à l'indentation. En Swift, travailler avec YAML n'est pas intégré nativement, donc on utilise des bibliothèques comme `Yams` pour le parsing et la génération.

## See Also
- Yams GitHub : https://github.com/jpsim/Yams
- The Official YAML Website : https://yaml.org
- Swift Package Manager : https://swift.org/package-manager/
