---
title:                "Sjekke om en mappe eksisterer"
html_title:           "TypeScript: Sjekke om en mappe eksisterer"
simple_title:         "Sjekke om en mappe eksisterer"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hvorfor

Å sjekke om en mappe eksisterer i TypeScript kan være nyttig hvis du ønsker å utføre ulike handlinger basert på om mappen allerede finnes eller ikke. Dette kan være spesielt nyttig når du jobber med filbehandling eller ønsker å organisere filer på en effektiv måte.

## Slik gjør du det

```TypeScript
if (fs.existsSync('./mappe')) {
  console.log("Mappen eksisterer!");
} else {
  console.log("Mappen eksisterer ikke.");
}
```

Det første du trenger å gjøre er å importere "fs" modulen, som står for "file system", ved å bruke "require" funksjonen. Deretter kan du bruke "existsSync" metoden for å sjekke om en mappe eksisterer ved å gi mappenavnet som et argument. Dette returnerer en boolean verdi, som kan brukes til å utføre ulike handlinger i koden.

## Dypdykk

Hvis du ønsker å sjekke om en mappe eksisterer på en annen plassering enn der koden din kjører, kan du bruke "path" modulen for å få den fulle stien til mappen og deretter bruke "existsSync" metoden med denne stien som argument.

```TypeScript
import path from "path";

const fullSti = path.join(__dirname, "mappe");
if (fs.existsSync(fullSti)) {
  console.log("Mappen eksisterer på en annen plassering!");
}
```

En annen ting du bør være oppmerksom på er at "existsSync" metoden returnerer "true" for både filer og mapper. Så hvis du ønsker å være sikker på at det er en mappe du sjekker, kan du bruke "statSync" metoden og sjekke at det er en mappe først.

## Se også

- [fs modulen i TypeScript](https://nodejs.org/api/fs.html#fs_fs_existssync_path)
- [path modulen i TypeScript](https://nodejs.org/api/path.html)
- [guide for filbehandling i TypeScript](https://www.digitalocean.com/community/tutorials/how-to-handle-file-uploads-in-node-js-with-multer)