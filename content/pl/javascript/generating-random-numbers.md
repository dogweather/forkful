---
title:    "Javascript: Generowanie losowych liczb"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego generowanie losowych liczb jest ważne?

Generowanie losowych liczb jest ważną częścią wielu programów i gier, ponieważ pozwala na stworzenie różnorodnych przebiegów i wyborów, co sprawia, że cały proces jest bardziej interesujący dla użytkowników. Może być również przydatne w testowaniu i debugowaniu kodu.

## Jak to zrobić?

Najprostszym sposobem na wygenerowanie losowej liczby w języku JavaScript jest użycie funkcji `Math.random()`, która zwraca wartość z zakresu od 0 do 1. Aby uzyskać liczbę z większego zakresu, można użyć funkcji `Math.floor()` lub `Math.ceil()`. Na przykład, aby wygenerować liczbę całkowitą z zakresu od 1 do 10, można użyć poniższego kodu:

```javascript
Math.floor(Math.random() * 10) + 1;
```

Można również utworzyć funkcję, która będzie zwracała losową liczbę z dowolnego zakresu, np.:

```javascript
function randomInRange(min, max){
  return Math.floor(Math.random() * (max - min + 1) + min);
}

randomInRange(1, 10); // zwróci losową liczbę z zakresu od 1 do 10
```

## Wnikliwa analiza

Generowanie losowych liczb w języku JavaScript może być trudne, ponieważ funkcja `Math.random()` używa tzw. "generatora liczb pseudolosowych", który w rzeczywistości nie generuje prawdziwie losowych liczb. Zamiast tego, jest to algorytm, który używa pewnych danych wejściowych, aby wyprodukować "losowe" liczby.

Jest to istotne, ponieważ oznacza to, że nie można polegać na funkcji `Math.random()` do celów związanych z bezpieczeństwem lub kryptografią. Jeśli potrzebujesz prawdziwej losowości, musisz skorzystać z zewnętrznej biblioteki lub API.

## Zobacz także

- Dokumentacja Math.random() w języku JavaScript: https://developer.mozilla.org/pl/docs/Web/JavaScript/Referencje/Obiekty/Math/random
- Sposoby generowania losowych liczb w języku JavaScript: https://medium.freecodecamp.org/how-to-generate-random-numbers-in-javascript-c39744c45e8a
- Wpływ generatorów liczb pseudolosowych na bezpieczeństwo w języku JavaScript: https://stackoverflow.com/questions/5651789/is-math-random-math-random-a-bad-way-to-generate-random-values