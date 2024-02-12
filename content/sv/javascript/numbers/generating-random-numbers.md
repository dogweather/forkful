---
title:                "Generera slumptal"
aliases:
- sv/javascript/generating-random-numbers.md
date:                  2024-01-27T20:34:46.076274-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generera slumptal"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att generera slumpmässiga tal i JavaScript är en teknik som används för att skapa oförutsägbarhet i applikationer, från spel som behöver slumpmässigt fiendebeteende till säkerhetsalgoritmer som kräver kryptografisk slumpmässighet. Denna förmåga är avgörande för att utveckla dynamiska användarupplevelser och säkra applikationer.

## Hur man gör:

### Enkel generering av slumpmässiga tal

Det enklaste sättet att generera ett slumpmässigt tal i JavaScript är att använda `Math.random()`. Denna funktion returnerar ett flyttal, pseudo-slumpmässigt tal i intervallet 0 (inklusive) till 1 (exklusive).

```javascript
let randomNumber = Math.random();
console.log(randomNumber);
```

### Generera ett slumpmässigt tal inom ett intervall

Ofta vill du ha ett slumpmässigt heltal inom ett specifikt intervall. Detta kan uppnås genom att skala och avrunda utmatningen från `Math.random()`.

```javascript
function getRandomInt(min, max) {
  min = Math.ceil(min);
  max = Math.floor(max);
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

console.log(getRandomInt(1, 100));
```

### Kryptografiskt säkra slumpmässiga tal

För applikationer som kräver en högre grad av slumpmässighet (t.ex. kryptografiska operationer), kan metoden `crypto.getRandomValues()` användas. Detta ger kryptografisk slumpmässighet, till skillnad från de pseudo-slumpmässiga tal som genereras av `Math.random()`.

```javascript
(function generateSecureRandom() {
  let array = new Uint32Array(1);
  window.crypto.getRandomValues(array);
  console.log(array[0]);
})();
```

## Fördjupning

Historiskt sett har generering av slumpmässiga tal i JavaScript enbart varit beroende av funktionen `Math.random()`. Medan den är praktisk för de flesta vardagliga användningsområden, tillhandahåller dess algoritm, som typiskt är en variant av en pseudo-slumpmässig nummergenerator (PRNG) som Mersenne Twister, inte kryptografisk säkerhet.

Introduktionen av Web Cryptography API medförde metoden `crypto.getRandomValues()`, som erbjuder ett sätt att generera tal som är mycket mindre förutsägbara och lämpliga för säkerhetskänsliga applikationer. Denna metod drar nytta av de underliggande operativsystemets slumpmässighetskällor, som `/dev/random` på Unix/Linux, vilka är robustare och mer lämpliga för kryptografiska operationer.

Det är avgörande att välja rätt metod för uppgiften i fråga. `Math.random()` räcker för grundläggande behov som enkla spel, animationer eller något fall där slumpmässighetens kvalitet inte är kritisk. Däremot, för säkerhetsfunktioner, som återställning av lösenord eller alla kryptografiska operationer, är `crypto.getRandomValues()` ett bättre val på grund av dess överlägsna slumpmässighetskvalitet.

Noterbart är att `Math.random()` genererar tal med en känd bias i de flesta implementeringar, vilket innebär att vissa nummer är mer sannolika att förekomma än andra. Även om denna bias är minimal och ofta omärklig för allmänna applikationer, diskvalificerar det `Math.random()` från att användas i någon kryptografisk kontext eller applikationer där rättvisa är avgörande, som online-spel.

Sammanfattningsvis, medan JavaScripts inbyggda funktioner för att generera slumpmässiga tal täcker ett brett behovsspektrum, är förståelsen av skillnaderna och begränsningarna hos varje metod avgörande för deras lämpliga användning.
