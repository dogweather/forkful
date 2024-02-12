---
title:                "Tarkistetaan, onko hakemisto olemassa"
aliases:
- /fi/typescript/checking-if-a-directory-exists.md
date:                  2024-02-03T19:08:53.087265-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tarkistetaan, onko hakemisto olemassa"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä ja miksi?
Hakemiston olemassaolon tarkistaminen TypeScriptillä on oleellinen osa tiedostonhallintatehtäviä, kuten tiedostoista lukemista tai niihin tietojen kirjoittamista, varmistaen, että toimenpiteet suoritetaan ainoastaan validioiden hakemistojen kanssa. Tämä toiminto on kriittinen välttääkseen virheet, jotka syntyvät yrittäessä päästä käsiksi tai manipuloida olemattomia hakemistoja.

## Kuinka:

TypeScript, kun sitä ajetaan Node.js-ympäristössä, mahdollistaa hakemiston olemassaolon tarkistamisen käyttämällä `fs`-moduulia, joka tarjoaa `existsSync()`-funktion tai asynkronisen `access()`-funktion yhdistettynä `constants.F_OK`:n kanssa.

### Käyttäen `fs.existsSync()`:

```typescript
import { existsSync } from 'fs';

const directoryPath = './path/to/directory';

if (existsSync(directoryPath)) {
  console.log('Hakemisto on olemassa.');
} else {
  console.log('Hakemistoa ei ole olemassa.');
}
```

### Käyttäen `fs.access()` yhdessä `fs.constants.F_OK` kanssa:

```typescript
import { access, constants } from 'fs';

const directoryPath = './path/to/directory';

access(directoryPath, constants.F_OK, (err) => {
  if (err) {
    console.log('Hakemistoa ei ole olemassa.');
    return;
  }
  console.log('Hakemisto on olemassa.');
});
```

**Esimerkkituloste** molemmille metodeille, olettaen että hakemisto on olemassa:
```
Hakemisto on olemassa.
```

Ja jos sitä ei ole:
```
Hakemistoa ei ole olemassa.
```

### Käyttäen kolmannen osapuolen kirjastoa - `fs-extra`:

`fs-extra` on suosittu kolmannen osapuolen kirjasto, joka parantaa sisäänrakennettua `fs`-moduulia ja tarjoaa kätevämpiä funktioita.

```typescript
import { pathExists } from 'fs-extra';

const directoryPath = './path/to/directory';

pathExists(directoryPath).then(exists => {
  console.log(`Hakemisto on olemassa: ${exists}`);
});
```

**Esimerkkituloste** kun hakemisto on olemassa:
```
Hakemisto on olemassa: true
```

Ja jos sitä ei ole:
```
Hakemisto on olemassa: false
```
