---
title:    "Javascript: Sjekke om en mappe eksisterer"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

# Hvorfor

Det å sjekke om en mappe eksisterer kan være en viktig del av programmering fordi det gir deg muligheten til å håndtere tilfeller der en mappe ikke er tilgjengelig eller er skrevet feil i koden din.

# Hvordan du gjør det

For å sjekke om en mappe eksisterer i JavaScript, kan du bruke `fs.existsSync`-funksjonen. Denne funksjonen tar inn en bane som parameter og sjekker om en mappe med den gitte banen eksisterer eller ikke. Hvis mappen eksisterer, returneres `true`, ellers returneres `false`.

```Javascript
const fs = require('fs');

const path = 'min/mappe/sti';

if (fs.existsSync(path)) {
  console.log('Mappen eksisterer!');
} else {
  console.log('Mappen eksisterer ikke.');
}
```

Utskriften av dette vil være avhengig av om mappen faktisk eksisterer eller ikke. Hvis mappen eksisterer, vil utskriften være "Mappen eksisterer!", ellers vil den være "Mappen eksisterer ikke." Dette kan også kombineres med andre funksjoner for å håndtere mer komplekse situasjoner.

# Dypdykk

Sjekking av eksistensen til en mappe kan også være nyttig når du skal lese eller skrive filer i den mappen. Hvis mappen ikke eksisterer, vil forsøk på å lese eller skrive filer i den feile. Derfor kan det være nyttig å først sjekke om mappen eksisterer før du prøver å jobbe med filene i den.

En annen viktig ting å huske på er at `fs.existsSync`-funksjonen sjekker kun om en mappe eksisterer, ikke om den er lesbar eller skrivbar. Det kan være lurt å kombinere denne funksjonen med andre funksjoner for å sikre at du har tilgang til mappen og dens innhold.

# Se også

- [JavaScript fs-modulen](https://nodejs.org/api/fs.html)
- [Hvordan sjekke om en fil eller mappe eksisterer i Node.js](https://www.digitalocean.com/community/tutorials/how-to-check-if-a-file-or-directory-exists-in-node-js)