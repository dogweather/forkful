---
title:                "Travailler avec YAML"
html_title:           "Bash: Travailler avec YAML"
simple_title:         "Travailler avec YAML"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
Le YAML, c'est du texte pour structurer des données, genre config ou input pour nos apps. On le kiffe car il est lisible et simple à piger pour les humains.

## How to:
Pour tripatouiller du YAML en JS, on utilise souvent `js-yaml`. Faut d'abord l'installer :

```bash
npm install js-yaml
```

Après, c'est un jeu d'enfant pour l'importer et s'en servir :

```javascript
const yaml = require('js-yaml');
const fs = require('fs');

// Pour lire un fichier YAML et le convertir en JS
try {
  const doc = yaml.load(fs.readFileSync('/mon/dossier/config.yaml', 'utf8'));
  console.log(doc);
} catch (e) {
console.error(e);
}

// Pour faire l'inverse, de JS à YAML
const data = { clé: 'valeur', liste: [1, 2, 3] };

const yamlStr = yaml.dump(data);
fs.writeFileSync('/mon/dossier/config.yaml', yamlStr, 'utf8');
```

Ces exemples montrent comment lire un fichier YAML, le convertir en objet JavaScript, puis comment reprendre cet objet et l'écrire en format YAML.

## Deep Dive
Le YAML, ça date de 2001, conçu pour être plus lisible que le XML. Aujourd'hui, beaucoup l'utilisent pour Docker, Kubernetes, GitLab CI, etc. Si on compare avec JSON, YAML accepte les commentaires, c'est plus "relax" pour le formatage. En JS, faudrait aussi checker `json2yaml` et `yamljs`, mais `js-yaml` est roy pour sa sécurité et performance.

## See Also
- La doc officielle de YAML pour piger le format : https://yaml.org
- Repo `js-yaml` pour les détails et docs : https://github.com/nodeca/js-yaml
- Un petit comparo entre JSON et YAML : https://www.json2yaml.com/