---
title:                "TypeScript: Pisanie testów"
simple_title:         "Pisanie testów"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/writing-tests.md"
---

{{< edit_this_page >}}

## Dlaczego pisać testy w TypeScript?

Pisanie testów może wydawać się dodatkowym i niepotrzebnym krokiem w procesie tworzenia oprogramowania. Jednak, nie ma wątpliwości, że jest to niezbędna część pisania jakościowego i niezawodnego kodu. Poniżej przedstawione są powody, dla których warto zainwestować czas i wysiłek w pisanie testów w TypeScript.

- **Zapewnienie niezawodności aplikacji:** Testy pomagają wykryć błędy w kodzie, co przekłada się na większą niezawodność i stabilność aplikacji. Dzięki nim możesz uniknąć problemów i błędów w produkcji, co przekłada się na lepsze wrażenia użytkownika.
- **Łatwiejsze utrzymanie:** Pisanie testów jest również korzystne w dłuższej perspektywie. Gdy aplikacja rośnie w skali, łatwiej będzie wprowadzać zmiany i modyfikacje w kodzie, jeśli posiadasz solidną bazę testową.

Teraz, gdy już wiemy, dlaczego warto pisać testy w TypeScript, przejdźmy do praktycznego przewodnika.

## Jak pisać testy w TypeScript?

Testy można pisać w różny sposób w zależności od tego, co chcesz przetestować. Jedną z popularnych bibliotek do testowania w TypeScript jest [Jest](https://jestjs.io/). Jest to prosta i wydajna biblioteka, która oferuje wiele funkcji, takich jak asercje, mockowanie i grupowanie testów.

Aby zainstalować bibliotekę Jest, wystarczy uruchomić polecenie `npm install jest --save-dev` w konsoli lub terminalu. Następnie możesz utworzyć plik testowy z rozszerzeniem `.test.ts` i rozpocząć pisanie testów.

Poniższy przykład pokazuje, jak przetestować funkcję `increaseNumberByOne`:

```TypeScript
// plik z kodem do przetestowania

export function increaseNumberByOne(num): number {
  return num + 1;
}
```

```TypeScript
// plik z testami

import { increaseNumberByOne } from "./kod-do-przetestowania";

test("Powinien zwrócić zwiększoną liczbę o jeden", () => {
  expect(increaseNumberByOne(5)).toBe(6);
});
```

W powyższym przykładzie użyliśmy asercji `toBe`, aby sprawdzić, czy funkcja zwraca poprawny wynik. Jest to tylko jedna z wielu funkcji dostępnych w bibliotece Jest. Warto także zaznaczyć, że wartość zwrócona przez funkcję `increaseNumberByOne` musi być typu `number`, co możemy określić w deklaracji typów.

## Deep Dive (Głębszy przegląd)

Pisanie testów może być skomplikowanym procesem, ale dzięki praktyce i odpowiednim narzędziom możesz szybko stać się ekspertem w tej dziedzinie. Poniżej znajdują się kilka wskazówek, które mogą pomóc Ci w pisaniu lepszych testów:

- **Zajmij się testowaniem zanim zaczniesz kodować:** Zanim napiszesz nawet pierwszą linię kodu, warto zaplanować, jakie testy będą potrzebne do pokrycia funkcjonalności aplikacji. W ten sposób możesz uniknąć zbędnych testów lub pomyśleć o przypadkach, których nie uwzględniłeś wcześniej.
- **Użyj innych narzędzi do testowania:** Oprócz biblioteki Jest, istnieje wiele innych narzędzi do testowania w TypeScript, takich jak [Mocha](https://mochajs.org/) czy [Chai](https://www.chaijs.com/). Sprawdź różne opcje i wy