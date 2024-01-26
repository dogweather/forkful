---
title:                "Kirjoittaminen vakiovirheeseen"
html_title:           "Bash: Kirjoittaminen vakiovirheeseen"
simple_title:         "Kirjoittaminen vakiovirheeseen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Mitä ja miksi? Standard error (stderr) on stream, johon ohjelmat kirjoittavat virheviestit. Ohjelmoijat käyttävät sitä, koska virheet on hyvä erottaa normaalista tulosteesta (stdout), jolloin logit ja debuggaus helpottuvat.

## How to:
Koodiesimerkit ja näytetulostukset.

```TypeScript
// Kirjoitus standard erroriin
process.stderr.write('Tämä on virheviesti.\n');

// Konsolilogitus standard erroriin
console.error('Tämä on toinen virheviesti.');
```

Sample output:

```
Tämä on virheviesti.
Tämä on toinen virheviesti.
```

## Deep Dive:
Syväsukellus. Alun perin Unix-järjestelmissä oli käytössä kolme standardivirtaa: input (stdin), output (stdout) ja error (stderr). Ne mahdollistivat tiedon suuntaamisen ja käsittelyn ohjelmien välillä. Vaihtoehtoisia tapoja kirjoittaa virheviestejä ovat esim. lokeihin kirjoittaminen tiedostoihin tai kaukokirjautumispalveluihin. TypeScriptissä stderr-käsittelyn yksityiskohdat pohjautuvat Node.js:n `process`-objektin toimintoihin.

## See Also:
Katso myös.

- Node.js documentation on `process.stderr`: https://nodejs.org/api/process.html#process_process_stderr
- Understanding streams in Node.js: https://nodejs.dev/learn/nodejs-streams
- Effective logging in TypeScript: https://www.typescriptlang.org/docs/handbook/release-notes/typescript-3-1.html#logging
