---
aliases:
- /fr/javascript/working-with-toml/
date: 2024-01-26 04:23:16.733715-07:00
description: "TOML, abr\xE9viation de Tom's Obvious, Minimal Language, d\xE9finit\
  \ comment structurer les fichiers de configuration. Les programmeurs travaillent\
  \ avec TOML car\u2026"
lastmod: 2024-02-18 23:09:09.282229
model: gpt-4-0125-preview
summary: "TOML, abr\xE9viation de Tom's Obvious, Minimal Language, d\xE9finit comment\
  \ structurer les fichiers de configuration. Les programmeurs travaillent avec TOML\
  \ car\u2026"
title: Travailler avec TOML
---

{{< edit_this_page >}}

## Quoi et pourquoi ?
TOML, abréviation de Tom's Obvious, Minimal Language, définit comment structurer les fichiers de configuration. Les programmeurs travaillent avec TOML car il est facile à lire, à écrire et se mappe bien à une table de hachage, ce qui en fait un choix privilégié pour les configurations.

## Comment faire :
Pour travailler avec TOML en JavaScript, vous aurez besoin d'un analyseur comme `@iarna/toml`. Commencez par l'installer : `npm install @iarna/toml`. Ensuite, analysez une chaîne TOML en un objet JavaScript ou transformez un objet JavaScript en format TOML.

```javascript
const toml = require('@iarna/toml');

// Analyser la chaîne TOML en objet JS
const tomlStr = `
title = "Exemple TOML"

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
`;

const parsedData = toml.parse(tomlStr);
console.log(parsedData);

// Convertir l'objet JS en chaîne TOML
const jsObject = {
  title: "Exemple TOML",
  database: {
    server: "192.168.1.1",
    ports: [8001, 8001, 8002]
  }
};

const tomlString = toml.stringify(jsObject);
console.log(tomlString);
```

## Plongée profonde
TOML a été lancé pour la première fois en 2013 par Tom Preston-Werner, un co-fondateur de GitHub. Il a été conçu pour succéder à d'autres formats, comme INI, en étant plus standardisé et plus facile à analyser. JSON et YAML sont des alternatives, mais peuvent être trop complexes ou trop flexibles. L'avantage de TOML réside dans la configuration statique où un format simple et clair est préféré. Sa conception permet un mappage direct dans une table de hachage, avec des clés et des valeurs correspondant aux noms des propriétés et à leurs valeurs. Pour une adoption plus large, vous devrez peut-être intégrer des outils capables de convertir entre TOML et d'autres formats en raison du support variable de l'écosystème.

## Voir également
- Le dépôt GitHub officiel de TOML : https://github.com/toml-lang/toml
- Comparaison TOML vs YAML vs JSON : https://gist.github.com/oconnor663/9aeb4ed56394cb013a20
- Le paquet npm `@iarna/toml` : https://www.npmjs.com/package/@iarna/toml
