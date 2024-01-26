---
title:                "Travailler avec YAML"
html_title:           "Bash: Travailler avec YAML"
simple_title:         "Travailler avec YAML"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
Le YAML, c'est un format de sérialisation de données lisible par les humains. Les programmeurs l'utilisent pour configurer des projets, stocker des informations et faciliter la communication entre différentes parties d'un système.

## How to:
Installe `js-yaml`, une lib populaire pour YAML :

```bash
npm install js-yaml
```

Importe le module et lis un fichier YAML :

```TypeScript
import * as fs from 'fs';
import * as yaml from 'js-yaml';

try {
  const config = yaml.load(fs.readFileSync('config.yaml', 'utf8'));
  console.log(config);
} catch (e) {
  console.log(e);
}
```

Prenons `config.yaml` :

```yaml
version: 1
services:
  web:
    image: "my-web-app:latest"
    deploy:
      replicas: 2
```

Si tout va bien, ça va afficher :

```json
{ version: 1, services: { web: { image: 'my-web-app:latest', deploy: { replicas: 2 } } } }
```

## Deep Dive
YAML, acronyme de "YAML Ain't Markup Language", est né en 2001. Conçu pour surpasser les limitations de l'ancien JSON, le YAML est plus lisible et peut représenter des structures de données plus complexes. Alternatives? Le JSON et le XML sont connus, mais YAML reste le chouchou pour les fichiers de config. Niveau implémentation, YAML permet des références, l'héritage et d'autres fonctionnalités avancées grâce à son schéma flexible.

## See Also
- La spécification YAML officielle : https://yaml.org/spec/
- `js-yaml` GitHub repository: https://github.com/nodeca/js-yaml
- Tutoriel YAML approfondi : https://www.tutorialspoint.com/yaml/index.htm
