---
date: 2024-01-27 20:35:47.077997-07:00
description: "Jak to zrobi\u0107: W TypeScript, mo\u017Cesz generowa\u0107 losowe\
  \ liczby za pomoc\u0105 globalnego obiektu `Math`. Poni\u017Cej znajduj\u0105 si\u0119\
  \ praktyczne przyk\u0142ady demonstruj\u0105ce,\u2026"
lastmod: '2024-03-13T22:44:35.134746-06:00'
model: gpt-4-0125-preview
summary: "W TypeScript, mo\u017Cesz generowa\u0107 losowe liczby za pomoc\u0105 globalnego\
  \ obiektu `Math`."
title: Generowanie liczb losowych
weight: 12
---

## Jak to zrobić:
W TypeScript, możesz generować losowe liczby za pomocą globalnego obiektu `Math`. Poniżej znajdują się praktyczne przykłady demonstrujące, jak produkować losowe liczby dla różnych wymagań.

### Generowanie podstawowej losowej liczby
Aby wygenerować podstawową losową liczbę dziesiętną między 0 (włącznie) a 1 (wyłącznie), używasz `Math.random()`. Nie wymaga to żadnych dodatkowych manipulacji:

```TypeScript
const randomNumber = Math.random();
console.log(randomNumber);
```

To może wygenerować wartość taką jak `0.8995452185604771`.

### Generowanie losowej liczby całkowitej między dwoma wartościami
Kiedy potrzebujesz liczby całkowitej między dwoma konkretnymi wartościami, włączasz zarówno `Math.random()`, jak i pewne działania arytmetyczne:

```TypeScript
function getRandomInt(min: number, max: number): number {
  min = Math.ceil(min);
  max = Math.floor(max);
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

const randomInt = getRandomInt(1, 10);
console.log(randomInt);
```

To może wygenerować wartość całkowitą między 1 a 10, taką jak `7`.

### Generowanie unikalnego identyfikatora
Losowe liczby można łączyć z innymi metodami, aby tworzyć unikalne identyfikatory, na przykład prosty fragment generatora UUID:

```TypeScript
function generateUUID(): string {
    return 'xxxxyxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, (c) => {
        const r = Math.random() * 16 | 0, v = c == 'x' ? r : (r & 0x3 | 0x8);
        return v.toString(16);
    });
}

const uuid = generateUUID();
console.log(uuid);
```

To generuje ciąg przypominający UUID, taki jak `110e8400-e29b-41d4-a716-446655440000`.

## Dogłębne spojrzenie
Podstawowa metoda generowania losowych liczb w JavaScript, a więc i w TypeScript, `Math.random()`, polega na pseudolosowym generatorze liczb (PRNG). Ważne jest, aby zauważyć, że chociaż wyniki mogą wydawać się losowe, są one generowane przez deterministyczny algorytm oparty na początkowej wartości ziarnistej. Dlatego liczby produkowane przez `Math.random()` nie są prawdziwie losowe i nie powinny być używane do celów kryptograficznych.

Dla kryptograficznie bezpiecznych losowych liczb, Web Crypto API oferuje `crypto.getRandomValues()`, które jest dostępne w środowiskach wspierających standard Web Crypto, w tym nowoczesnych przeglądarkach i Node.js (za pośrednictwem modułu `crypto`). Oto szybki przykład ilustrujący jego użycie w TypeScript do generowania bezpiecznej losowej liczby w zakresie:

```TypeScript
function secureRandom(min: number, max: number): number {
    const array = new Uint32Array(1);
    window.crypto.getRandomValues(array);
    return min + (array[0] % (max - min + 1));
}

const secureRandNum = secureRandom(1, 100);
console.log(secureRandNum);
```

Ta metoda zapewnia silniejszy poziom losowości i jest bardziej odpowiednia dla aplikacji wymagających bezpieczeństwa. Jednakże jest również bardziej zasobożerna i może nie być konieczna do bardziej prozaicznych zadań, takich jak proste symulacje lub generowanie wartości losowych o niskim stopniu krytyczności.
