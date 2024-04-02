---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:14.805945-07:00
description: "\xC5 skrive en tekstfil i JavaScript handler ofte om \xE5 opprette og\
  \ lagre data i et enkelt, lesbart format for logging, eksport av brukerinndata eller\u2026"
lastmod: '2024-03-13T22:44:41.201660-06:00'
model: gpt-4-0125-preview
summary: "\xC5 skrive en tekstfil i JavaScript handler ofte om \xE5 opprette og lagre\
  \ data i et enkelt, lesbart format for logging, eksport av brukerinndata eller\u2026"
title: Skrive en tekstfil
weight: 24
---

## Hva & Hvorfor?
Å skrive en tekstfil i JavaScript handler ofte om å opprette og lagre data i et enkelt, lesbart format for logging, eksport av brukerinndata eller konfigurasjonsformål. Denne funksjonaliteten er avgjørende for applikasjoner som trenger å bevare data utover levetiden til applikasjonsprosessen, og tilbyr en måte å lagre og senere hente eller dele informasjon på.

## Hvordan:
I et Node.js-miljø kan du bruke den innebygde `fs` (File System) modulen for å skrive tekstfiler. Dette eksemplet demonstrerer skriving av tekst til en fil asynkront:

```javascript
const fs = require('fs');

const data = 'Hallo, Verden! Dette er tekst som skal skrives til en fil.';

fs.writeFile('example.txt', data, (err) => {
  if (err) {
    throw err;
  }
  console.log('Filen er skrevet.');
});
```

Eksempelutskrift:
```
Filen er skrevet.
```

For synkron filskriving, bruk `writeFileSync`:
```javascript
try {
  fs.writeFileSync('example.txt', data);
  console.log('Filen er skrevet.');
} catch (error) {
  console.error('Feil ved skriving av fil:', error);
}
```

I moderne nettlesere introduserer File System Access API muligheten til å lese og skrive filer. Imidlertid er bruken underlagt brukerens tillatelser. Her er hvordan du oppretter og skriver til en fil:

```javascript
if ('showSaveFilePicker' in window) {
  const handle = await window.showSaveFilePicker();
  const writable = await handle.createWritable();
  await writable.write('Hallo, Verden! Dette er tekstfilskriving i nettleser.');
  await writable.close();
}
```

For mer komplekse scenarier eller når du jobber med store filer, kan du velge tredjepartsbiblioteker som `FileSaver.js` for nettlesere:

```html
<script src="https://cdnjs.cloudflare.com/ajax/libs/FileSaver.js/2.0.2/FileSaver.min.js"></script>
<script>
  const blob = new Blob(["Hallo, Verden! Dette er tekst fra FileSaver.js."], {type: "text/plain;charset=utf-8"});
  saveAs(blob, "example.txt");
</script>
```

Husk at filskriving på klientsiden (i nettlesere) er begrenset på grunn av sikkerhetshensyn, og enhver operasjon som krever lagring på brukerens lokale disk vil vanligvis kreve deres uttrykkelige tillatelse.
