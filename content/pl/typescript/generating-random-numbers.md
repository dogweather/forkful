---
title:    "TypeScript: Tworzenie losowych liczb"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego

Wiele programów wymaga wykorzystywania losowych liczb, czy to do symulacji scenariuszy, czy też do generowania różnych danych. W przypadku projektów tworzonych w języku TypeScript, można wykorzystać wbudowaną funkcję do generowania losowych liczb. W tym artykule dowiesz się, jak wykorzystać ten mechanizm i jakie są jego możliwości.

## Jak

Generowanie losowych liczb w TypeScript jest bardzo proste i wymaga użycia wbudowanej funkcji `Math.random()`. Ta funkcja zwraca pseudolosową liczbę z przedziału 0 do 1. Następnie możemy przemnożyć tę wartość przez odpowiednią liczbę, aby uzyskać zakres liczb, który nas interesuje. Na przykład, jeśli chcemy wygenerować losową liczbę z przedziału od 1 do 10, możemy wykorzystać poniższy kod:

```TypeScript
const randomNumber = Math.random() * 10 + 1;
console.log(randomNumber);
```

W powyższym przykładzie, funkcja `Math.random()` zwróci wartość od 0 do 1, a następnie zostanie ona przemnożona przez 10 i dodana 1, co pozwoli nam uzyskać liczbę z zakresu od 1 do 10.

Możemy także wykorzystać funkcję `Math.floor()`, aby zaokrąglić wygenerowaną liczbę do najbliższej liczby całkowitej. W ten sposób otrzymamy losową liczbę całkowitą z zakresu, na przykład:

```TypeScript
const randomInteger = Math.floor(Math.random() * 10) + 1;
console.log(randomInteger);
```

Powyższy kod zwróci losową liczbę całkowitą z przedziału od 1 do 10.

## Deep Dive

Funkcja `Math.random()` jest oparta na algorytmie wykorzystującym liczbę pseudolosową, która jest generowana w sposób deterministyczny. Oznacza to, że przy użyciu tej samej wartości początkowej, zostanie wygenerowany ten sam ciąg liczb. W związku z tym, funkcja ta nie jest zalecana do wykorzystywania w celach bezpieczeństwa, takich jak generowanie haseł czy tokenów uwierzytelniających.

Jeśli chcesz mieć większą kontrolę nad generowaniem liczb losowych, można wykorzystać biblioteki zewnętrzne, takie jak `random-js`, które oferują rozmaite algorytmy dla generowania liczb pseudolosowych. Możesz także wykorzystać generator liczb losowych oferowany przez bibliotekę `nanoid`, który pozwala na generowanie unikalnych identyfikatorów, wykorzystując zarówno wartości pseudolosowe, jak i losowe.

## Zobacz także

- [Dokumentacja na temat funkcji Math.random() w języku TypeScript](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-1.html#assoc-comm-index-access)
- [Biblioteka random-js](https://github.com/davidbau/seedrandom)
- [Biblioteka nanoid](https://github.com/ai/nanoid)