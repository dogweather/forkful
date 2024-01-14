---
title:                "TypeScript: Pisanie testów"
programming_language: "TypeScript"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/writing-tests.md"
---

{{< edit_this_page >}}

## Dlaczego pisanie testów jest ważne?

Pisanie testów jest kluczowym elementem w procesie tworzenia oprogramowania, ponieważ pomaga zapewnić wysoką jakość i niezawodność kodu. Testy pozwalają programistom automatycznie sprawdzać, czy ich kod działa poprawnie oraz wykrywać błędy i ewentualne problemy. Pomagają również w utrzymaniu czytelności kodu i ułatwiają wprowadzanie zmian.

## Jak pisać testy w TypeScript

Pierwszym krokiem w pisaniu testów w TypeScript jest zainstalowanie narzędzia do testowania, takiego jak Jasmine, Mocha lub Jest. Następnie należy stworzyć nowy plik testowy z rozszerzeniem .spec.ts. W takim pliku należy zdefiniować testy przy pomocy funkcji describe, it i expect. Poniżej przedstawiam przykładowy kod testu w TypeScript dla funkcji dodawania.

```
// Importowanie funkcji z pliku z kodem
import { add } from './filename.ts';

// Opis testu
describe('Test dla funkcji dodawania', () => {

  // Kod testu
  it('Powinno zwrócić poprawny wynik dla dwóch liczb dodatnich', () => {

    // Oczekiwany wynik
    const expectedResult = 5;

    // Wywołanie funkcji
    const result = add(2, 3);

    // Porównanie wyniku
    expect(result).toEqual(expectedResult);
  });
});
```

Po napisaniu testów należy je uruchomić przy użyciu narzędzia do testowania. Jeśli testy przejdą pozytywnie, oznacza to, że tworzony kod jest poprawny i nie zawiera błędów.

## Deep Dive: Wskazówki dotyczące pisania testów

Poniżej przedstawiam kilka wskazówek, które pomogą Ci w pisaniu testów w TypeScript:

- Upewnij się, że nazwy funkcji i zmiennych użytych w teście są czytelne, aby ułatwić zrozumienie testu.
- Nie twórz zbyt długich testów. Staraj się testować pojedyncze funkcjonalności zamiast całego modułu.
- Używaj funkcji beforeEach i afterEach do wykonywania kodu przed i po każdym teście. Dzięki temu unikniesz duplikacji kodu i uprościsz proces testowania.

## Zobacz też

- [Dokumentacja TypeScript](https://www.typescriptlang.org/docs/)
- [Narzędzia do testowania w TypeScript](https://medium.com/javascript-in-plain-english/top-5-testing-tools-for-typescript-68b5cd15b952)
- [Materiały szkoleniowe TypeScript z testami](https://www.udacity.com/course/typescript-testing--ud015)