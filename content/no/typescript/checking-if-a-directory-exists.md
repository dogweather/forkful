---
title:    "TypeScript: Sjekke om en mappe eksisterer"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why

Å sjekke om en mappe eksisterer kan være en viktig del av programmering. Det lar deg sikre deg at nødvendige filer og mapper er på plass før du fortsetter med kritiske operasjoner.

## How To

Å sjekke om en mappe eksisterer er en enkel prosess i TypeScript. Du trenger bare å bruke `fs.existsSync()`-funksjonen. Her er et eksempel som vil sjekke om en mappe ved navn "bilder" eksisterer:

```TypeScript
if(fs.existsSync("./bilder")){
    console.log("Mappen finnes!");
} else {
    console.log("Mappen finnes ikke.");
}
```

Det vil også være nyttig å vise en liste over filer og mapper i en eksisterende mappe. Her er et eksempel som gjør nettopp det:

```TypeScript
fs.readdirSync("./bilder").forEach((file) => {
    console.log(file);
})
```

Output:

```
bilde1.jpg
bilde2.jpg
bilde3.jpg
```

## Deep Dive

Når du bruker `fs.existsSync()`, må du passe på at stien du oppgir er korrekt. Hvis stien er relativ, vil den være relatert til den gjeldende arbeidsmappe. Hvis du vil sjekke en sti som er relatert til filen som kjører, kan du bruke `__dirname`-variabelen.

En annen ting å merke seg er at `fs.existsSync()` bare sjekker om en mappe eksisterer, men ikke om den er tilgjengelig for skriving eller lesing. For å sjekke om en mappe kan skrives til, kan du for eksempel bruke `fs.accessSync()`-funksjonen.

## Se Også

- [Node.js FileSystem-dokumentasjon](https://nodejs.org/api/fs.html)
- [Sjekke eksistens av filer og mapper i TypeScript](https://www.fyneworks.com/2012/03/29/check-if-a-file-or-directory-exists-while-using-typescript/)
- [En grundig guide til filbehandling i TypeScript](https://medium.com/@romanonthego/file-handling-with-typescript-e4249eeba5a0)