---
title:    "Javascript: Konwertowanie ciągu znaków na małe litery"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwersja ciągu znaków na małe litery może być przydatna w wielu sytuacjach, szczególnie w przypadku pracy z tekstem. Może to pomóc w porównywaniu tekstu, eliminowaniu błędów związanych z wielkością liter oraz ułatwieniu procesów filtrowania i sortowania danych. 

## Jak to zrobić

W języku Javascript istnieje kilka sposobów na konwersję ciągu znaków na małe litery. Jednym z najprostszych sposobów jest użycie metody `toLowerCase()`. Przykładowy kod wyglądałby następująco:

```Javascript
let text = "PRZYKŁADOWY TEKST";
let lowercaseText = text.toLowerCase();

console.log(lowercaseText);
// Output: przykładowy tekst
```

Innym sposobem jest użycie metody `replace()` wraz z wyrażeniem regularnym, aby zamienić wszystkie duże litery na małe. Przykład takiego kodu może wyglądać następująco:

```Javascript
let text = "PRZYKŁADOWY TEKST";
let lowercaseText = text.replace(/[A-Z]/g, letter => letter.toLowerCase());

console.log(lowercaseText);
// Output: przykładowy tekst
```

## Głębsza analiza

Warto wybrać odpowiednią metodę konwersji w zależności od potrzeb i kontekstu. Metoda `toLowerCase()` jest szybsza i prostsza w użyciu, ale może nie być odpowiednia, jeśli potrzebujemy zachować pewną strukturę lub format tekstu. Natomiast użycie `replace()` pozwala na większą kontrolę nad procesem konwersji poprzez wykorzystanie wyrażenia regularnego.

Kluczowa jest również znajomość różnic pomiędzy małymi i dużymi literami w różnych językach. Na przykład w języku polskim litera "Ś" po konwersji na małą literę jest "ś", a nie "s" jak w języku angielskim. Dlatego warto dokładnie przeanalizować, jakie efekty będzie miała konwersja na małe litery na różnych językach.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o konwersji ciągów znaków w języku Javascript, polecam zapoznać się z następującymi artykułami i dokumentacją:

- [MDN Web Docs - Metoda `toLowerCase()`](https://developer.mozilla.org/pl/docs/Web/JavaScript/Referencje/Obiekty/String/toLowerCase)
- [MDN Web Docs - Metoda `replace()`](https://developer.mozilla.org/pl/docs/Web/JavaScript/Referencje/Obiekty/String/replace)
- [W3Schools - Konwersja liter w języku Javascript](https://www.w3schools.com/jsref/jsref_tolowercase.asp)
- [Dokumentacja języka Javascript](https://developer.mozilla.org/pl/docs/Web/JavaScript)

Dzięki znajomości tych technik, będą Państwo w stanie efektywniej pracować z tekstem w swoich projektach w języku Javascript.