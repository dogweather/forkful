---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:06:11.228880-07:00
description: "TOML, qui signifie \"Tom's Obvious, Minimal Language\" (le Langage Minimal\
  \ et \xC9vident de Tom), est un format de fichier de configuration facile \xE0 lire\
  \ gr\xE2ce\u2026"
lastmod: '2024-03-13T22:44:57.214118-06:00'
model: gpt-4-0125-preview
summary: "TOML, qui signifie \"Tom's Obvious, Minimal Language\" (le Langage Minimal\
  \ et \xC9vident de Tom), est un format de fichier de configuration facile \xE0 lire\
  \ gr\xE2ce \xE0 sa s\xE9mantique claire."
title: Travailler avec TOML
weight: 39
---

## Quoi et pourquoi ?

TOML, qui signifie "Tom's Obvious, Minimal Language" (le Langage Minimal et Évident de Tom), est un format de fichier de configuration facile à lire grâce à sa sémantique claire. Les programmeurs l'utilisent souvent pour les fichiers de configuration des applications car il est simple et lisible par l'humain, rendant la gestion des paramètres et configurations d'application homogène à travers différents environnements.

## Comment faire :

Puisque Google Apps Script est essentiellement du JavaScript avec accès à la suite d'apps de Google, travailler avec TOML directement dans Google Apps Script requiert un peu d'ingéniosité. Google Apps Script ne supporte pas nativement l'analyse de TOML, mais vous pouvez tirer parti de bibliothèques JavaScript ou écrire un analyseur simple pour les besoins de base.

Analysons une chaîne de configuration TOML simple en exemple :

```javascript
// Chaîne TOML
var tomlString = `
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
`;

// Une fonction simple d'analyseur TOML vers JSON
function parseTOML(tomlStr) {
  var result = {};
  var currentSection = result;
  tomlStr.split(/\r?\n/).forEach(ligne => {
    ligne = ligne.trim();
    if (ligne.startsWith('[')) { // Nouvelle section
      var nomSection = ligne.replace(/\[|\]/g, '');
      result[nomSection] = {};
      currentSection = result[nomSection];
    } else if (ligne) {
      var keyValue = ligne.split('=').map(partie => partie.trim());
      var cle = keyValue[0];
      var valeur = eval(keyValue[1]); // Utiliser eval par simplicité ; attention dans le code de production
      currentSection[cle] = valeur;
    }
  });
  return result;
}

// Tester l'analyseur
var configObject = parseTOML(tomlString);
console.log(configObject);

```

Un exemple de sortie de `console.log` ressemblerait à un objet JSON, rendant plus simple l'accès aux propriétés de configuration dans Google Apps Script :

```json
{
  "database": {
    "server": "192.168.1.1",
    "ports": [8001, 8001, 8002],
    "connection_max": 5000,
    "enabled": true
  }
}
```

## Exploration approfondie

TOML a été créé par Tom Preston-Werner, l'un des fondateurs de GitHub, pour être plus convivial pour les humains que JSON pour les fichiers de configuration tout en conservant la capacité d'être analysé sans ambiguïté. Il vise à être aussi simple que possible, un objectif qui s'aligne bien avec l'éthique de nombreux projets de développement qui s'efforcent de simplifier et de rendre leurs bases de code lisibles.

Dans le contexte de Google Apps Script, utiliser TOML peut introduire une certaine charge de travail, étant donné l'absence de support direct et la nécessité d'analyser manuellement ou via des bibliothèques tierces. Pour des projets plus petits ou ceux qui ne sont pas profondément intégrés dans l'écosystème de Google, des alternatives telles que JSON ou même de simples structures de paires clé-valeur dans les propriétés de script pourraient suffire et être plus simples à mettre en œuvre. Cependant, pour les applications qui privilégient les fichiers de configuration conviviaux pour les humains et qui sont déjà engagées à utiliser TOML, intégrer l'analyse TOML par des scripts personnalisés ajoute une couche utile de flexibilité et de maintenabilité sans s'éloigner des paradigmes de configuration préférés.
