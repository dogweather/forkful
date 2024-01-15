---
title:                "Wyodrębnianie podciągów"
html_title:           "Javascript: Wyodrębnianie podciągów"
simple_title:         "Wyodrębnianie podciągów"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy zdarzyło Ci się kiedykolwiek potrzebować wyodrębnić fragment tekstu ze stringa w Javascript? Jeśli tak, to masz szczęście, ponieważ w tym artykule dowiesz się, jak łatwo i wygodnie wyodrębnić substringi w aktualnej wersji języka Javascript.

Wyodrębnianie substringów jest bardzo przydatne podczas manipulacji tekstem, na przykład podczas walidacji formularzy lub przetwarzania danych. Może to również pomóc w użyciu funkcji, które wymagają poszczególnych części tekstu, takich jak wyszukiwanie lub podział.

## Jak to zrobić

Aby wyodrębnić substring w Javascript, musimy użyć metody `substring()` lub `slice()`. Oba metody przyjmują dwa parametry: początkowy indeks i końcowy indeks.

Przykładowo, jeśli chcemy wyodrębnić substring "programowanie" z napisu "Artykuł o programowaniu", możemy użyć metody `substring()` w ten sposób:

```Javascript
let napis = "Artykuł o programowaniu";
let substring = napis.substring(10, 21);
console.log(substring); // "programowanie"
```

Jeśli chcemy wyodrębnić tylko niektóre znaki z napisu, możemy użyć metody `slice()` w ten sposób:

```Javascript
let napis = "Lorem ipsum dolor sit amet";
let substring = napis.slice(6, 11);
console.log(substring); // "ipsum"
```

W obu powyższych przykładach drugi indeks jest liczbą większą niż pierwszy, co oznacza, że zostanie wyodrębniony fragment od pierwszego indeksu włącznie do drugiego indeksu bez włączenia.

W przypadku użycia metody `substring()` lub `slice()` z tylko jednym parametrem, zostaną wyodrębnione znaki od tego indeksu włącznie do końca napisu.

Możemy również użyć liczb ujemnych jako indeksów, co spowoduje zliczenie znaków od końca napisu wstecz. Przykładowo, `-3` oznacza trzeci znak od końca napisu.

## Głęboki zanurzenie

Metoda `substring()` jest zawsze odporne na błędy, oznacza to, że jeśli drugi indeks jest mniejszy niż pierwszy, to metoda będzie działać tak, jakby podano je w odwrotnej kolejności.

Z drugiej strony, metoda `slice()` jest trochę bardziej zaawansowana i może obsługiwać ujemne liczby, co pozwala na wygodniejsze wyodrębnianie fragmentów tekstu z końca napisu.

Warto również wspomnieć, że w przypadku użycia ujemnych indeksów, metoda `slice()` zwróci fragment od końca napisu, w przeciwieństwie do metody `substring()`, która zwróci fragment od początku.

## Zobacz również

- Dokumentacja Javascript na temat metod `substring()` i `slice()`: https://developer.mozilla.org/pl/docs/Web/JavaScript/Reference/Global_Objects/String
- Przydatne wskazówki i przykłady wyodrębniania substringów: https://www.w3schools.com/jsref/jsref_substring.asp