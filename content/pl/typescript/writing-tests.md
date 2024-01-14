---
title:    "TypeScript: Pisanie testów"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/writing-tests.md"
---

{{< edit_this_page >}}

## Dlaczego

Napisanie testów jest kluczowym elementem każdego projektu programistycznego. Testy pozwalają upewnić się, że nasz kod działa poprawnie i zgodnie z oczekiwaniami. Są również niezwykle przydatne w wykrywaniu błędów i zapobieganiu regresji w kodzie. W tym wpisie dowiesz się dlaczego warto pisać testy i jak w prosty sposób możesz to zrobić w języku TypeScript.

## Jak To Zrobić

Pisanie testów w TypeScript może wydawać się skomplikowane, jednak w rzeczywistości jest to dość proste. Najpierw musimy zainstalować odpowiednią bibliotekę testową za pomocą menedżera pakietów, na przykład npm lub Yarn. W przypadku TypeScript możemy korzystać z popularnej biblioteki o nazwie Jest.

Po zainstalowaniu biblioteki, możemy przystąpić do tworzenia testów. Stwórzmy prosty plik o nazwie `Sum.test.ts` i dodajmy do niego kod:

```TypeScript
import sum from '../src/sum';

test('correctly adds 2 numbers', () => {
  expect(sum(2, 3)).toBe(5);
});

test('correctly adds a negative number', () => {
  expect(sum(2, -5)).toBe(-3);
});
```

W powyższym przykładzie tworzymy dwa testy funkcji `sum()`, która oblicza sumę dwóch liczb. Wykorzystujemy tutaj funkcję `expect()` z biblioteki Jest, aby sprawdzić czy wynik jest zgodny z oczekiwaniami. Następnie uruchamiamy testy za pomocą polecenia `npm run test` lub `yarn run test` i powinniśmy otrzymać poprawny wynik.

## Deep Dive

Pisanie testów może być bardziej zaawansowane niż pokazane w prostym przykładzie powyżej. W rzeczywistości możemy testować nie tylko małe funkcje, ale również interakcje z bazą danych, wywołania API czy interfejsy użytkownika. Jest to możliwe dzięki wykorzystaniu tzw. mocków, które pozwalają nam symulować różne scenariusze i sprawdzać czy nasz kod zachowuje się poprawnie w różnych warunkach.

Warto również wspomnieć o terminie TDD (ang. Test Driven Development), który jest popularną strategią wytwarzania oprogramowania. Polega ona na pisaniu testów przed napisaniem właściwego kodu, co pozwala na lepsze zrozumienie wymagań i zapobiega powstawaniu błędów.

## Zobacz też

- [Dokumentacja biblioteki Jest](https://jestjs.io/)
- [Wprowadzenie do testów w TypeScript](https://dev.to/maxpou/introduction-to-testing-typescript-with-jest-2nae)
- [Poradnik TDD dla początkujących](https://www.freecodecamp.org/news/learn-test-driven-development-in-10-minutes-7920b2aa1ec4/)