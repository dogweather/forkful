---
date: 2024-01-26 04:27:04.148803-07:00
description: "Wie geht das: Zuerst ben\xF6tigst du einen TOML-Parser. `@iarna/toml`\
  \ ist eine beliebte Wahl. Installiere es mit npm: `npm install @iarna/toml --save`.\
  \ So\u2026"
lastmod: '2024-03-13T22:44:53.654040-06:00'
model: gpt-4-0125-preview
summary: "Zuerst ben\xF6tigst du einen TOML-Parser."
title: Arbeiten mit TOML
weight: 39
---

## Wie geht das:
Zuerst benötigst du einen TOML-Parser. `@iarna/toml` ist eine beliebte Wahl. Installiere es mit npm: `npm install @iarna/toml --save`. So liest du eine TOML-Datei und parst sie zu einem JavaScript-Objekt:

```typescript
import * as fs from 'fs';
import toml from '@iarna/toml';

const tomlContent = fs.readFileSync('config.toml', 'utf-8');
const parsedData = toml.parse(tomlContent);

console.log(parsedData);
```
Wenn `config.toml` beinhaltet:
```
[server]
port = 8080
```
Wäre die Ausgabe:
```
{ server: { port: 8080 } }
```
Und, in eine TOML-Datei zu schreiben ist genauso unkompliziert:
```typescript
import * as fs from 'fs';
import { stringify } from '@iarna/toml';

const obj = { server: { port: 8080 } };
const tomlString = stringify(obj);
fs.writeFileSync('config.toml', tomlString);
``` 
Dieser Code schreibt das Objekt im TOML-Format in `config.toml`.

## Tiefere Einblicke
TOML wurde von Tom Preston-Werner, dem Mitbegründer von GitHub, um 2013 als Antwort auf die von ihm wahrgenommenen Einschränkungen anderer Formate wie INI oder YAML geschaffen. Es ist darauf ausgelegt, eindeutig und leicht in Datenstrukturen zu parsen zu sein, daher ein Favorit für Konfigurationsdateien. Alternativen wie JSON fehlen Kommentare, während YAML komplexer ist. TOML glänzt in seiner Einfachheit und seiner Fähigkeit, komplexe Datenhierarchien klar darzustellen.

Unter der Haube, wenn du TOML in TypeScript parst, konvertierst du textuelle Daten in ein strukturiertes Format, das die Sprache manipulieren kann. Dies beinhaltet Lexing (rohen Text in Token umwandeln) und Parsen (eine interne Datenstruktur aufbauen); `@iarna/toml` handhabt beides nahtlos. Die Emoji-Unterstützung ist eine lustige Note, die TOMLs benutzerzentrierten Ansatz zeigt.

## Siehe auch
- Offizielle TOML-Spezifikation: https://toml.io/en/
- `@iarna/toml` Paket: https://www.npmjs.com/package/@iarna/toml
- Vergleiche zwischen TOML, YAML und JSON: https://blog.bitsrc.io/choosing-the-right-configuration-file-format-toml-vs-yaml-vs-json-71b5be8968ea
