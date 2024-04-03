---
date: 2024-01-26 04:27:05.157728-07:00
description: "TOML, abr\xE9viation de Tom's Obvious, Minimal Language (langage minimal\
  \ et \xE9vident de Tom), est un format de s\xE9rialisation de donn\xE9es semblable\
  \ \xE0 JSON ou\u2026"
lastmod: '2024-03-13T22:44:57.462589-06:00'
model: gpt-4-0125-preview
summary: "TOML, abr\xE9viation de Tom's Obvious, Minimal Language (langage minimal\
  \ et \xE9vident de Tom), est un format de s\xE9rialisation de donn\xE9es semblable\
  \ \xE0 JSON ou YAML."
title: Travailler avec TOML
weight: 39
---

## Comment faire :
Tout d'abord, vous aurez besoin d'un analyseur TOML. `@iarna/toml` est un choix populaire. Installez-le avec npm : `npm install @iarna/toml --save`. Voici comment lire un fichier TOML et le parser en un objet JavaScript :

```typescript
import * as fs from 'fs';
import toml from '@iarna/toml';

const contenuToml = fs.readFileSync('config.toml', 'utf-8');
const donneesParsees = toml.parse(contenuToml);

console.log(donneesParsees);
```
Si `config.toml` contient :
```
[server]
port = 8080
```
Le résultat sera :
```
{ server: { port: 8080 } }
```
Et, écrire dans un fichier TOML est tout aussi simple :
```typescript
import * as fs from 'fs';
import { stringify } from '@iarna/toml';

const obj = { server: { port: 8080 } };
const chaineToml = stringify(obj);
fs.writeFileSync('config.toml', chaineToml);
``` 
Exécuter ce code écrit l'objet dans `config.toml` au format TOML.

## Plongée Profonde
TOML a été créé par Tom Preston-Werner, co-fondateur de GitHub, vers 2013 en réponse aux limitations qu'il percevait dans d'autres formats comme INI ou YAML. Il est conçu pour être sans ambiguïté et facile à parser en structures de données, donc un favori pour les fichiers de configuration. Des alternatives comme JSON manquent de commentaires, tandis que YAML est plus complexe. TOML brille par sa simplicité et sa capacité à représenter clairement des hiérarchies de données complexes.

Sous le capot, lorsque vous parsez TOML en TypeScript, vous convertissez des données textuelles en un format structuré que le langage peut manipuler. Cela implique le lexing (transformation du texte brut en jetons) et le parsing (construction d'une structure de données interne) ; `@iarna/toml` gère les deux de manière transparente. Le support des emoji est une touche amusante, montrant l'approche centrée sur l'utilisateur de TOML.

## Voir également
- Spécification officielle de TOML : https://toml.io/en/
- paquet `@iarna/toml` : https://www.npmjs.com/package/@iarna/toml
- Comparaisons entre TOML, YAML et JSON : https://blog.bitsrc.io/choosing-the-right-configuration-file-format-toml-vs-yaml-vs-json-71b5be8968ea
