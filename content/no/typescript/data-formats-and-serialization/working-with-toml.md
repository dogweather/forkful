---
date: 2024-01-26 04:27:09.037599-07:00
description: "TOML, som st\xE5r for Tom's Obvious, Minimal Language, er et data serialiseringsformat\
  \ likt JSON eller YAML. Programm\xF8rer bruker det p\xE5 grunn av dets\u2026"
lastmod: '2024-03-13T22:44:40.555181-06:00'
model: gpt-4-0125-preview
summary: "TOML, som st\xE5r for Tom's Obvious, Minimal Language, er et data serialiseringsformat\
  \ likt JSON eller YAML. Programm\xF8rer bruker det p\xE5 grunn av dets\u2026"
title: Jobbe med TOML
---

{{< edit_this_page >}}

## Hva & Hvorfor?
TOML, som står for Tom's Obvious, Minimal Language, er et data serialiseringsformat likt JSON eller YAML. Programmører bruker det på grunn av dets menneskelesbarhet og enkle kartlegging til datatyper, noe som gjør det til et førstevalg for konfigurasjonsfiler og datautveksling.

## Hvordan:
Først trenger du en TOML-parser. `@iarna/toml` er et populært valg. Installer det med npm: `npm install @iarna/toml --save`. Slik leser du en TOML-fil og parser den til et JavaScript-objekt:

```typescript
import * as fs from 'fs';
import toml from '@iarna/toml';

const tomlContent = fs.readFileSync('config.toml', 'utf-8');
const parsedData = toml.parse(tomlContent);

console.log(parsedData);
```
Hvis `config.toml` inneholder:
```
[server]
port = 8080
```
Vil utdata være:
```
{ server: { port: 8080 } }
```
Og, å skrive til en TOML-fil er like rett frem:
```typescript
import * as fs from 'fs';
import { stringify } from '@iarna/toml';

const obj = { server: { port: 8080 } };
const tomlString = stringify(obj);
fs.writeFileSync('config.toml', tomlString);
``` 
Å kjøre denne koden skriver objektet til `config.toml` i TOML-format.

## Dypdykk
TOML ble opprettet av Tom Preston-Werner, medgrunnleggeren av GitHub, rundt 2013 som et svar på de begrensningene han oppfattet med andre formater som INI eller YAML. Det er designet for å være utvetydig og enkelt å parse inn i datastrukturer, derav, et favorittvalg for konfigurasjonsfiler. Alternativer som JSON mangler kommentarer, mens YAML er mer kompleks. TOML utmerker seg med sin enkelhet og evnen til å tydelig representere komplekse datahierarkier.

Under panseret, når du parser TOML i TypeScript, konverterer du tekstuell data til et strukturert format som språket kan manipulere. Dette innebærer lekseanalyse (å omdanne rå tekst til tokens) og parsing (å bygge en intern datastruktur); `@iarna/toml` håndterer begge sømløst. Emoji-støtten er en morsom touch, som viser TOMLs brukersentrerte tilnærming.

## Se Også
- TOML Offisiell Spesifikasjon: https://toml.io/en/
- `@iarna/toml` pakke: https://www.npmjs.com/package/@iarna/toml
- Sammenligninger mellom TOML, YAML og JSON: https://blog.bitsrc.io/choosing-the-right-configuration-file-format-toml-vs-yaml-vs-json-71b5be8968ea
