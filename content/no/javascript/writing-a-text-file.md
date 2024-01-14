---
title:                "Javascript: Skrive en tekstfil"
simple_title:         "Skrive en tekstfil"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Hvorfor

Å skrive en tekstfil er en viktig ferdighet for enhver JavaScript-programmer. Det lar deg lagre og organisere data på en enkel måte, og kan være nyttig for å lagre brukerpreferanser, lagre data fra skjemaer eller generere rapporter.

# Hvordan

Å skrive en tekstfil er en relativt enkel prosess i JavaScript. Først må vi opprette en fil ved hjelp av `fs`-modulen. Vi bruker `createWriteStream()`-funksjonen og gir den navnet på filen vi ønsker å opprette, for eksempel "data.txt".

```Javascript
const fs = require('fs');
const file = fs.createWriteStream('data.txt');
```

Nå kan vi skrive data til filen ved hjelp av `.write()`-funksjonen. Vi kan skrive enkel tekst eller variabler til filen, for eksempel:

```Javascript
file.write('Dette er en tekst som blir skrevet til filen\n');
file.write(`${variable1} er en variabel som blir skrevet til filen`);
```

Vi må også huske å lukke filen når vi er ferdige med å skrive data ved å bruke `.end()`-funksjonen.

```Javascript
file.end();
```

Det er viktig å merke seg at ved å bruke `.write()`-funksjonen overskriver vi ikke eksisterende data i filen, men legger til ny data på slutten av filen.

# Dypdykk

Noen ganger ønsker vi kanskje å skrive større mengder data til en fil, for eksempel en JSON-struktur eller en tabell med data. I slike tilfeller kan det være nyttig å bruke `JSON.stringify()`-funksjonen for å konvertere dataene til en tekststreng før vi skriver dem til filen.

```Javascript
const data = { name: 'John', age: 25 };
file.write(JSON.stringify(data));
```

Vi kan også bruke `fs`-modulens `appendFile()`-funksjon hvis vi ønsker å legge til data til en eksisterende fil i stedet for å skrive over den.

```Javascript
fs.appendFile('data.txt', 'Denne teksten legges til på slutten av filen', (err) => {
  if (err) throw err;
  console.log('Data lagt til i filen');
});
```

# Se også

- Node.js `fs`-modulen dokumentasjon: https://nodejs.org/api/fs.html
- `.createWriteStream()`-funksjonen: https://nodejs.org/api/fs.html#fs_fs_createwritestream_path_options
- `.write()`-funksjonen: https://nodejs.org/api/stream.html#stream_writable_write_chunk_encoding_callback
- `.end()`-funksjonen: https://nodejs.org/api/stream.html#stream_writable_end_chunk_encoding_callback
- `JSON.stringify()`-funksjonen: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON/stringify
- `.appendFile()`-funksjonen: https://nodejs.org/api/fs.html#fs_fs_appendfile_path_data_options_callback