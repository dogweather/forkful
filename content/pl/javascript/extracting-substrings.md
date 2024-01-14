---
title:    "Javascript: Wydobywanie podciągów"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego

Istnieje wiele powodów dla których programiści mogą potrzebować wydobyć podłańcuchy (substrings) z większych ciągów znaków w swoim kodzie JavaScript. Może to być potrzebne do analizy danych, wyciągania określonych informacji lub po prostu do manipulacji tekstem. Poniżej przedstawione zostaną przykłady w jaki sposób można wykorzystać metodę substring w praktyce.

## Jak to zrobić

 W JavaScript istnieją różne metody do wydobywania podłańcuchów, jednak jedną z najczęściej używanych jest metoda `substring ()`. Pozwala ona określając początek i koniec podłańcucha, na wydzielenie go z większego ciągu znaków. Poniżej znajduje się przykład, który pokazuje jak uzyskać podłańcuch od 6 do 11 znaku z tekstu:

```Javascript
let tekst = "To jest przykładowy tekst";
let podlacuch = tekst.substring(6, 11);
console.log(podlacuch); // wyświetli: jest 
```

W powyższym przykładzie, użyliśmy metody `substring()` na zmiennej `tekst`, podając argumenty 6 i 11, co oznacza, że wybrane zostaną znaki od 6 do 11, a następnie wynik zostaje przypisany do zmiennej `podlacuch`, którą wyświetlamy na konsoli.

Można również użyć `substring()` do wydobywania podłańcuchów na podstawie określonego znaku lub ciągu znaków znajdującego się w tekście. Na przykład, jeśli chcemy uzyskać podłańcuch zaczynający się od słowa "przykładowy", możemy użyć tego kodu:

```Javascript
let tekst = "To jest przykładowy tekst";
let podlacuch = tekst.substring(tekst.indexOf("przykładowy")); // znajduje indeks pierwszego wystąpienia słowa "przykładowy"
console.log(podlacuch); // wyświetli: przykładowy tekst 
```

Inną przydatną metodą jest `slice()`, która działa w podobny sposób do `substring()`, ale pozwala na podanie tylko jednego indeksu jako argumentu. Jeśli podamy tylko jeden indeks, zostanie utworzony podłańcuch od tego indeksu do końca tekstu. Przykład:

```Javascript
let tekst = "To jest przykładowy tekst";
let podlacuch = tekst.slice(8); // utworzy podłańcuch od 8 indeksu do końca tekstu
console.log(podlacuch); // wyświetli: przykładowy tekst
```

## Głębsza analiza

Metody `substring()` i `slice()` opisane powyżej są bardzo podstawowe, jednak w JavaScript istnieje także wiele innych sposobów na wydobywanie podłańcuchów. Na przykład, można wykorzystać wyrażenia regularne lub zastosować różne funkcje do manipulacji tekstem, takie jak `split()` czy `replace()`.

Podczas manipulacji tekstami, ważne jest także pamiętać o tym, że ciągi znaków są niezmienialne (immutable), co oznacza, że metody te nie zmieniają oryginalnego tekstu, lecz zwracają nowy podłańcuch.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej na temat eksplorowania podłańcuchów w JavaScript, polecamy zapoznać się z poniższymi linkami:

- Dokumentacja MDN na temat metody `substring()`: https://developer.mozilla.org/pl/docs/Web/JavaScript/Referencje/Obiekty/String/substring
- Poradnik dotyczący wydobywania podłańcuchów w JavaScript: https