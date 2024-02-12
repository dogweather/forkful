---
title:                "Willekeurige getallen genereren"
aliases: - /nl/javascript/generating-random-numbers.md
date:                  2024-01-28T22:01:11.478472-07:00
model:                 gpt-4-0125-preview
simple_title:         "Willekeurige getallen genereren"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/javascript/generating-random-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Het genereren van willekeurige getallen in JavaScript is een techniek die wordt gebruikt om onvoorspelbaarheid te creëren in applicaties, van spellen die willekeurig vijandig gedrag nodig hebben tot beveiligingsalgoritmen die cryptografische willekeur vereisen. Deze mogelijkheid is cruciaal voor het ontwikkelen van dynamische gebruikerservaringen en veilige applicaties.

## Hoe te:

### Basisgeneratie van Willekeurige Getallen

De meest eenvoudige manier om een willekeurig getal in JavaScript te genereren is het gebruik van `Math.random()`. Deze functie geeft een drijvendekommagetall, pseudo-willekeurig getal terug in het bereik 0 (inclusief) tot 1 (exclusief).

```javascript
let randomNumber = Math.random();
console.log(randomNumber);
```

### Genereren van een Willekeurig Getal binnen een Bereik

Vaak wil je een willekeurig geheel getal binnen een specifiek bereik. Dit kan worden bereikt door het output van `Math.random()` te schalen en af te ronden.

```javascript
function getRandomInt(min, max) {
  min = Math.ceil(min);
  max = Math.floor(max);
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

console.log(getRandomInt(1, 100));
```

### Cryptografisch Veilige Willekeurige Getallen

Voor applicaties die een hogere mate van willekeur vereisen (bijv. cryptografische operaties), kan de methode `crypto.getRandomValues()` worden gebruikt. Dit biedt cryptografische willekeur, in tegenstelling tot de pseudo-willekeurige getallen gegenereerd door `Math.random()`.

```javascript
(function generateSecureRandom() {
  let array = new Uint32Array(1);
  window.crypto.getRandomValues(array);
  console.log(array[0]);
})();
```

## Diepgaande Verkenning

Historisch gezien was de generatie van willekeurige getallen in JavaScript volledig afhankelijk van de functie `Math.random()`. Hoewel handig voor de meeste casual gebruikssituaties, zorgt zijn algoritme, typisch een variant van een pseudorandom number generator (PRNG) zoals Mersenne Twister, niet voor cryptografische veiligheid.

De introductie van de Web Cryptography API bracht de methode `crypto.getRandomValues()`, die een manier biedt om getallen te genereren die veel minder voorspelbaar zijn en geschikt voor beveiligingsgevoelige toepassingen. Deze methode maakt gebruik van de onderliggende willekeurbronnen van het besturingssysteem, zoals `/dev/random` op Unix/Linux, die robuuster zijn en geschikt voor cryptografische operaties.

Het is cruciaal om de juiste methode voor de taak bij de hand te kiezen. `Math.random()` volstaat voor basisbehoeften zoals eenvoudige spellen, animaties, of elk geval waar de kwaliteit van willekeur niet kritiek is. Echter, voor beveiligingsfuncties, zoals wachtwoord reset tokens of elke cryptografische operatie, is `crypto.getRandomValues()` de betere keuze vanwege zijn superieure willekeurkwaliteit.

Opvallend is dat `Math.random()` in de meeste implementaties getallen genereert met een bekende bias, wat betekent dat sommige getallen waarschijnlijker zijn dan andere. Ook al is deze bias minimaal en vaak onmerkbaar voor algemene toepassingen, het diskwalificeert `Math.random()` voor gebruik in elke cryptografische context of applicaties waar eerlijkheid cruciaal is, zoals online gokken.

Concluderend, hoewel de ingebouwde functies van JavaScript voor het genereren van willekeurige getallen een breed scala aan behoeften dekken, is het essentieel om de verschillen en beperkingen van elke methode te begrijpen voor hun passende toepassing.
