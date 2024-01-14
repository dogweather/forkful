---
title:    "TypeScript: Generowanie losowych liczb"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Dlaczego

Generowanie losowych liczb jest ważnym aspektem w programowaniu w języku TypeScript. Może to być przydatne w różnych zastosowaniach, takich jak symulacja losowych wydarzeń, testowanie aplikacji, czy generowanie unikalnych identyfikatorów.

## Jak to zrobić

Aby wygenerować losową liczbę z odpowiedniego zakresu, możemy użyć funkcji `Math.random()` w połączeniu z odpowiednimi operatorami arytmetycznymi. Na przykład, jeśli chcemy wygenerować liczbę całkowitą z zakresu od 1 do 10, możemy użyć poniższego kodu:

```TypeScript
let randomNumber = Math.floor(Math.random() * 10) + 1;
console.log(randomNumber);
```

Otrzymamy w ten sposób liczbę całkowitą z zakresu 1-10. Możemy również wykorzystać tę samą metodę do wygenerowania losowego znaku z tablicy, na przykład:

```TypeScript
let characters = ["a", "b", "c", "d", "e"];
let randomCharacter = characters[Math.floor(Math.random() * characters.length)];
console.log(randomCharacter);
```

W ten sposób otrzymamy losowo wybrany znak z tablicy.

## Głębszy wykład

W języku TypeScript istnieje również wbudowana klasa `Random` w module `crypto`, która umożliwia generowanie bezpiecznych i losowych liczb. Ta klasa wykorzystuje funkcję szyfrującą aby zapewnić większą losowość.

Dodatkowo, jeśli potrzebujemy wygenerować wiele liczb w szybszy sposób, możemy użyć biblioteki TypeScript o nazwie `random-js`, która dostarcza wiele zaawansowanych funkcji generujących losowe wartości z różnych rodzajów danych.

## Zobacz również

- [Dokumentacja TypeScript - Math.random()](https://www.typescriptlang.org/docs/handbook/basic-types.html#number)
- [Dokumentacja TypeScript - Crypto](https://nodejs.org/api/crypto.html)
- [Biblioteka random-js dla TypeScript](https://www.npmjs.com/package/random-js)