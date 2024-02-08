---
title:                "Työskentely JSON:n kanssa"
aliases:
- fi/typescript/working-with-json.md
date:                  2024-02-03T19:24:43.080864-07:00
model:                 gpt-4-0125-preview
simple_title:         "Työskentely JSON:n kanssa"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?

Työskentely JSONin (JavaScript Object Notation) kanssa sisältää JSON-datan jäsentämistä edestakaisin TypeScriptissä käytettävään muotoon. Ohjelmoijat tekevät tämän, jotta he voivat helposti käsitellä, tallentaa tai lähettää rakenteellista dataa, sillä JSON on kevyt, tekstipohjainen ja helposti luettavissa sekä ihmisten että koneiden toimesta.

## Kuinka:

### JSONin jäsentäminen TypeScript-objektiksi
JSON-merkkijonon muuntamiseksi TypeScript-objektiksi käytät `JSON.parse()`-metodia. Tämä on hyödyllistä, kun vastaanotat JSON-dataa verkkopalvelimelta tai luet JSON-tiedostoa.

```typescript
const jsonStr = '{"name": "John Doe", "age": 30}';
const obj = JSON.parse(jsonStr);

console.log(obj.name); // Tuloste: John Doe
```

### TypeScript-objektin muuntaminen JSON-merkkijonoksi
TypeScript-objektin muuntamiseksi JSON-merkkijonoksi käytät `JSON.stringify()`-metodia. Tämä on erityisen hyödyllistä, kun sinun tarvitsee lähettää dataa verkkopalvelimeen.

```typescript
const person = {
  name: "Jane Doe",
  age: 25,
};

const jsonStr = JSON.stringify(person);

console.log(jsonStr); // Tuloste: {"name":"Jane Doe","age":25}
```

### Työskentely rajapintojen kanssa
Voit määritellä TypeScript-rajapintoja toimiaksesi saumattomasti JSON-datan kanssa varmistaen objektiisi rakenteen.

```typescript
interface Person {
  name: string;
  age: number;
}

const jsonStr = '{"name": "Alex", "age": 28}';
const person: Person = JSON.parse(jsonStr);

console.log(person.age); // Tuloste: 28
```

### Suosittujen kolmannen osapuolen kirjastojen käyttö
Monimutkaisemmissa skenaarioissa, kuten skeeman validoimisessa tai muuntamisessa, saatat turvautua kirjastoihin, kuten `class-transformer` tai `AJV` (Another JSON Schema Validator).

#### class-transformer
Tämä kirjasto voi muuntaa tavalliset objektit luokkainstansseiksi ja päinvastoin, mikä on hyödyllistä tyypin tarkistuksessa ja manipuloinnissa.

```typescript
import "reflect-metadata";
import { plainToClass } from "class-transformer";
import { Person } from "./person";

const jsonStr = '{"name": "Mia", "age": 22}';
const person = plainToClass(Person, JSON.parse(jsonStr));

console.log(person instanceof Person); // Tuloste: true
console.log(person.name); // Tuloste: Mia
```

#### AJV
AJV on kirjasto, joka mahdollistaa nopean JSON-skeeman validoinnin. Tämä tarkoittaa, että voit validoida JSON-objektit ennalta määriteltyjä skeemoja vastaan.

```typescript
import Ajv from "ajv";

const ajv = new Ajv();

const schema = {
  type: "object",
  properties: {
    name: { type: "string" },
    age: { type: "number" },
  },
  required: ["name", "age"],
  additionalProperties: false,
};

const validate = ajv.compile(schema);
const valid = validate({ name: "Tom", age: 24 });

console.log(valid); // Tuloste: true
if (!valid) console.log(validate.errors);
```

Näiden työkalujen ja tekniikoiden avulla voit tehokkaasti käsitellä JSON-dataa TypeScript-sovelluksissasi, varmistaen datan eheyden ja hyödyntäen TypeScriptin tehokasta tyyppijärjestelmää.
