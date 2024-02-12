---
title:                "Travailler avec YAML"
aliases:
- /fr/typescript/working-with-yaml.md
date:                  2024-02-03T19:26:49.619597-07:00
model:                 gpt-4-0125-preview
simple_title:         "Travailler avec YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
YAML, un langage de sérialisation de données conçu pour être convivial, est souvent utilisé pour les fichiers de configuration, la messagerie entre processus, et le stockage de données. Les programmeurs se tournent vers YAML pour sa lisibilité et sa facilité d'utilisation, en particulier lorsqu'il s'agit de données structurées complexes, ce qui en fait un excellent choix pour les applications développées en TypeScript.

## Comment faire :
Travailler avec YAML en TypeScript implique généralement l'analyse du contenu YAML en objets JavaScript et, éventuellement, la conversion d'objets JavaScript en retour vers YAML. Cela nécessite un analyseur ; un choix populaire est `js-yaml`, une bibliothèque qui peut facilement être intégrée dans les projets TypeScript.

### Installer js-yaml
Premièrement, ajoutez `js-yaml` à votre projet :

```bash
npm install js-yaml
```

### Analyser le YAML en Objet JavaScript
Imaginez que vous avez un fichier YAML `config.yaml` avec le contenu suivant :

```yaml
database:
  host: localhost
  port: 5432
  username: user
  password: pass
```

Vous pouvez lire et analyser ce fichier en un objet JavaScript comme suit :

```typescript
import * as fs from 'fs';
import * as yaml from 'js-yaml';

// Charger et analyser le fichier YAML
const fileContents = fs.readFileSync('./config.yaml', 'utf8');
const data = yaml.load(fileContents) as Record<string, any>;

console.log(data);
```

**Exemple de sortie :**

```json
{
  "database": {
    "host": "localhost",
    "port": 5432,
    "username": "user",
    "password": "pass"
  }
}
```

### Convertir l'Objet JavaScript en YAML
Si vous avez besoin de faire l'inverse et de convertir un objet JavaScript en chaîne YAML, vous pouvez utiliser `js-yaml` comme suit :

```typescript
import * as yaml from 'js-yaml';

const obj = {
  title: "Example",
  is_published: true,
  author: {
    name: "Jane Doe",
    age: 34
  }
};

const yamlStr = yaml.dump(obj);
console.log(yamlStr);
```

**Exemple de sortie :**

```yaml
title: Example
is_published: true
author:
  name: Jane Doe
  age: 34
```

Ce fragment convertit un objet JavaScript en chaîne YAML et l'affiche. En pratique, vous pourriez écrire cela de nouveau dans un fichier ou l'utiliser dans d'autres parties de votre application.
