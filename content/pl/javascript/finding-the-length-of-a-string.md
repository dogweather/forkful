---
title:    "Javascript: Znajdowanie długości ciągu znaków"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Dlaczego

Dlaczego powinniśmy zwracać uwagę na długość ciągu w języku Javascript? W wielu przypadkach, stosowanie operacji na długości ciągu jest niezbędne do poprawnego wykonania złożonych skryptów. Może być również przydatne do walidacji danych wprowadzonych przez użytkownika lub do analizy tekstu.

## Jak to zrobić

Aby uzyskać długość ciągu w języku Javascript, możemy wykorzystać metodę `length` na zmiennej zawierającej nasz ciąg. Poniższy przykład pokazuje jak wygląda to w praktyce:

```Javascript
let ciag = "To jest nasz ciąg tekstu";
console.log(ciag.length);
```

Output: 26

Jeśli chcemy pobrać długość ciągu bez uwzględniania spacji, możemy użyć metody `trim` aby usunąć białe znaki na początku i końcu ciągu, a następnie zastosować metodę `length`. Przykład:

```Javascript
let ciag = "To jest nasz ciąg tekstu";
console.log(ciag.trim().length);
```

Output: 22

## Głębsza analiza

Poza podstawowym użyciem, warto zapoznać się z dodatkowymi metodami związanymi z długością ciągów w języku Javascript. Jedną z nich jest metoda `substr`, która pozwala na pobranie określonej liczby znaków z danego ciągu, zaczynając od określonego indeksu. Przykład:

```Javascript
let ciag = "To jest nasz ciąg tekstu";
console.log(ciag.substr(3, 8));
```

Output: jest nasz

Kolejną przydatną metodą jest `split`, która umożliwia podzielenie ciągu na tablicę, wykorzystując określony separator. Przykład:

```Javascript
let ciag = "To,jest,nasz,ciąg,tekstu";
console.log(ciag.split(","));
```

Output: ["To", "jest", "nasz", "ciąg", "tekstu"]

W przypadku potrzeby uzyskania dostępu do poszczególnych znaków ciągu, możemy również skorzystać z notacji nawiasów kwadratowych i indeksów wewnątrz nich, np. `ciag[3]` zwróci znak znajdujący się na 4. pozycji w ciągu.

## Zobacz również

- [Dokumentacja MDN - String length](https://developer.mozilla.org/pl/docs/Web/JavaScript/Referencje/String/length)
- [Dokumentacja MDN - String substr](https://developer.mozilla.org/pl/docs/Web/JavaScript/Referencje/String/substr)
- [Dokumentacja MDN - String split](https://developer.mozilla.org/pl/docs/Web/JavaScript/Referencje/String/split)