---
date: 2024-01-26 04:27:10.848815-07:00
description: "Jak to zrobi\u0107: Na pocz\u0105tek potrzebny b\u0119dzie parser TOML.\
  \ `@iarna/toml` to popularny wyb\xF3r. Zainstaluj go przy pomocy npm: `npm install\
  \ @iarna/toml\u2026"
lastmod: '2024-03-13T22:44:35.164015-06:00'
model: gpt-4-0125-preview
summary: "Na pocz\u0105tek potrzebny b\u0119dzie parser TOML."
title: Praca z TOML
weight: 39
---

## Jak to zrobić:
Na początek potrzebny będzie parser TOML. `@iarna/toml` to popularny wybór. Zainstaluj go przy pomocy npm: `npm install @iarna/toml --save`. Oto jak odczytać plik TOML i przekształcić go w obiekt JavaScript:

```typescript
import * as fs from 'fs';
import toml from '@iarna/toml';

const tomlContent = fs.readFileSync('config.toml', 'utf-8');
const parsedData = toml.parse(tomlContent);

console.log(parsedData);
```
Jeśli `config.toml` zawiera:
```
[server]
port = 8080
```
Wynik będzie:
```
{ server: { port: 8080 } }
```
I zapisywanie do pliku TOML jest równie proste:
```typescript
import * as fs from 'fs';
import { stringify } from '@iarna/toml';

const obj = { server: { port: 8080 } };
const tomlString = stringify(obj);
fs.writeFileSync('config.toml', tomlString);
``` 
Uruchomienie tego kodu zapisze obiekt do `config.toml` w formacie TOML.

## W głębi
TOML został stworzony przez Toma Preston-Wernera, współzałożyciela GitHuba, około 2013 roku jako odpowiedź na ograniczenia, które dostrzegał w innych formatach, takich jak INI czy YAML. Został zaprojektowany, aby być jednoznaczny i łatwy do przekształcenia w struktury danych, stąd jest ulubieńcem plików konfiguracyjnych. Alternatywy, takie jak JSON, nie mają komentarzy, podczas gdy YAML jest bardziej skomplikowany. TOML wyróżnia się prostotą i zdolnością do jasnego przedstawiania złożonych hierarchii danych.

Pod maską, kiedy parsujesz TOML w TypeScript, konwertujesz dane tekstowe na strukturalny format, którym język może manipulować. Dotyczy to leksera (zmiana surowego tekstu na tokeny) i parsowania (tworzenie wewnętrznej struktury danych); `@iarna/toml` radzi sobie z oboma bezproblemowo. Wsparcie dla emoji to zabawny dodatek, pokazujący zorientowane na użytkownika podejście TOML.

## Zobacz także
- Oficjalna specyfikacja TOML: https://toml.io/en/
- Pakiet `@iarna/toml`: https://www.npmjs.com/package/@iarna/toml
- Porównania między TOML, YAML, i JSON: https://blog.bitsrc.io/choosing-the-right-configuration-file-format-toml-vs-yaml-vs-json-71b5be8968ea
