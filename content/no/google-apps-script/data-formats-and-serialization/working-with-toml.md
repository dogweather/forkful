---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:06:21.206397-07:00
description: "TOML, som st\xE5r for Toms Opplagte, Minimale Spr\xE5k, er et konfigurasjonsfilformat\
  \ som er lett \xE5 lese p\xE5 grunn av sin klare semantikk. Programmerere bruker\u2026"
lastmod: '2024-03-13T22:44:40.341473-06:00'
model: gpt-4-0125-preview
summary: "TOML, som st\xE5r for Toms Opplagte, Minimale Spr\xE5k, er et konfigurasjonsfilformat\
  \ som er lett \xE5 lese p\xE5 grunn av sin klare semantikk. Programmerere bruker\u2026"
title: Arbeider med TOML
---

{{< edit_this_page >}}

## Hva & Hvorfor?

TOML, som står for Toms Opplagte, Minimale Språk, er et konfigurasjonsfilformat som er lett å lese på grunn av sin klare semantikk. Programmerere bruker det ofte for konfigurasjonsfiler i applikasjoner fordi det er ukomplisert og menneskelesbart, noe som gjør håndteringen av applikasjonsinnstillinger og konfigurasjoner sømløs på tvers av forskjellige miljøer.

## Hvordan:

Siden Google Apps Script i bunn og grunn er JavaScript med tilgang til Googles app-samling, krever arbeid med TOML direkte innen Google Apps Script litt oppfinnsomhet. Google Apps Script støtter ikke naturlig TOML-analyse, men du kan dra nytte av JavaScript-biblioteker eller skrive en enkel tolker for grunnleggende behov.

La oss analysere en enkel TOML konfigurasjonsstreng som et eksempel:

```javascript
// TOML-streng
var tomlString = `
[database]
server = "192.168.1.1"
porter = [ 8001, 8001, 8002 ]
maks_tilkobling = 5000
aktivert = true
`;

// En enkel TOML til JSON tolkefunksjon
function parseTOML(tomlStr) {
  var resultat = {};
  var gjeldendeSeksjon = resultat;
  tomlStr.split(/\r?\n/).forEach(linje => {
    linje = linje.trim();
    if (linje.startsWith('[')) { // Ny seksjon
      var seksjonsnavn = linje.replace(/\[|\]/g, '');
      resultat[seksjonsnavn] = {};
      gjeldendeSeksjon = resultat[seksjonsnavn];
    } else if (linje) {
      var nøkkelVerdi = linje.split('=').map(del => del.trim());
      var nøkkel = nøkkelVerdi[0];
      var verdi = eval(nøkkelVerdi[1]); // Bruker eval for enkelthets skyld; vær oppmerksom i produksjonskode
      gjeldendeSeksjon[nøkkel] = verdi;
    }
  });
  return resultat;
}

// Test tolkeren
var konfigObjekt = parseTOML(tomlString);
console.log(konfigObjekt);

```

Eksempelutdata fra `console.log` ville ligne et JSON-objekt, noe som gjør det enklere å få tilgang til konfigurasjonsegenskapene innen Google Apps Script:

```json
{
  "database": {
    "server": "192.168.1.1",
    "porter": [8001, 8001, 8002],
    "maks_tilkobling": 5000,
    "aktivert": true
  }
}
```

## Dypdykk

TOML ble skapt av Tom Preston-Werner, en av grunnleggerne av GitHub, for å være mer menneskevennlig enn JSON for konfigurasjonsfiler samtidig som det er mulig å tolkes utvetydig. Det sikter på å være så enkelt som mulig, et mål som passer fint sammen med ethoset til mange utviklingsprosjekter som streber etter enkelhet og lesbarhet i sine kodebaser.

I konteksten av Google Apps Script, kan bruk av TOML introdusere noe overhead, gitt mangel på direkte støtte og nødvendigheten for å analysere det manuelt eller gjennom tredjepartsbiblioteker. For mindre prosjekter eller de som ikke er dypt integrert i Googles økosystem, kan alternativer som JSON eller til og med enkle nøkkel-verdi par strukturer i scriptegenskaper være tilstrekkelige og mer rett frem å implementere.  Men, for applikasjoner som prioriterer menneskevennlige konfigurasjonsfiler og allerede er forpliktet til TOML, legger integrering av TOML-tolkning gjennom tilpassede script til et nyttig lag av fleksibilitet og vedlikeholdbarhet uten å avvike fra de foretrukne konfigurasjonsparadigmene.
