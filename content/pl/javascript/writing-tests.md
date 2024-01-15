---
title:                "Pisanie testów"
html_title:           "Javascript: Pisanie testów"
simple_title:         "Pisanie testów"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/writing-tests.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie testów w Javascript może wydawać się zbędne lub czasochłonne, ale rzeczywistość jest taka, że jest to bardzo ważny krok w procesie tworzenia aplikacji. Testy pozwalają zweryfikować poprawność kodu i upewnić się, że wszelkie zmiany nie wpłynęły negatywnie na działanie programu. Jest to szczególnie istotne w większych projektach, gdzie proste błędy mogą mieć poważne konsekwencje.

## Jak to zrobić?

```Javascript
// Przykładowa funkcja do testowania
function dodaj(a, b) {
  return a + b
}

// Przykład testu dla powyższej funkcji
describe("dodaj()", () => {
  it("powinno prawidłowo zsumować dwie liczby", () => {
    expect(dodaj(2, 4)).toBe(6)
  })
})
```

W powyższym przykładzie użyliśmy biblioteki do testów o nazwie Jest, ale istnieje wiele innych narzędzi dostępnych dla języka Javascript. Testowanie funkcji może wydawać się proste, ale można także pisać testy dla bardziej skomplikowanych części kodu, takich jak interakcje z bazą danych czy funkcje asynchroniczne.

## Deep Dive

Podczas pisania testów warto pamiętać o kilku rzeczach. Po pierwsze, testy powinny być jak najbardziej niezależne od siebie, aby można było w łatwy sposób je modyfikować i dodawać nowe. Dlatego dobrze jest podzielić testy na mniejsze jednostki, np. testowanie pojedynczych funkcji zamiast całych modułów. Po drugie, testy powinny sprawdzać poprawność działania kodu, a nie samą jego składnię. Nie jest to miejsce na poprawianie błędów, należy to zrobić w samym kodzie.

## Zobacz także

- [Jest](https://jestjs.io/)
- [Mocha](https://mochajs.org/)
- [Chai](https://www.chaijs.com/)
- [Czym są testy jednostkowe i dlaczego są ważne](https://blog.devskiller.com/pl/testy-jednostkowe-w-javascript/)
- [Javascript Testing Best Practices](https://medium.com/welldone-software/an-overview-of-javascript-testing-7ce7298b9870)