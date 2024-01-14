---
title:    "Javascript: Konwertowanie ciągu znaków na małe litery"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Dlaczego

Każdy programista pracujący w języku Javascript spotkał się z koniecznością konwertowania znaków na małe litery. Może to być potrzebne do porównywania danych, filtrowania list czy też po prostu dla estetyki kodu. W tym artykule dowiesz się, jak w łatwy sposób przekonwertować string na małe litery.

## Jak to zrobić?

Konwersja stringa na małe litery może być wykonana za pomocą wbudowanej funkcji `toLowerCase()`:

```Javascript
let zdanie = "PROGRAMOWANIE JEST WSPANIAŁE";

console.log(zdanie.toLowerCase());
```

W powyższym przykładzie użyliśmy metody `toLowerCase()` na zmiennej `zdanie`, która zawiera zdanie napisane wielkimi literami. Wynik wyświetlony w konsoli będzie wyglądał następująco:

```Javascript
"programowanie jest wspaniałe"
```

Możemy również użyć tej metody na stałych tekstowych, na przykład:

```Javascript
console.log("JĘZYK JAVASCRIPT".toLowerCase());
```

Od teraz będziemy wykorzystywać tylko i wyłącznie małe litery!

## Deep Dive

W języku Javascript istnieje wiele metod do konwersji ciągu znaków, jednakże `toLowerCase()` jest jedną z najprostszych i najczęściej stosowanych. Warto jednak zauważyć, że nie jest ona w 100% niezawodna, ponieważ może wpływać na wydajność naszego kodu. Jeśli wywołamy tę metodę na bardzo długim stringu, może to spowodować opóźnienia w działaniu programu. Dlatego warto przemyśleć, czy innię sposoby nie będą lepsze dla naszego konkretnego przypadku.

## Zobacz również

- [String.prototype.charAt()](https://developer.mozilla.org/pl/docs/Web/JavaScript/Referencje/String/charAt)
- [String.prototype.toUpperCase()](https://developer.mozilla.org/pl/docs/Web/JavaScript/Referencje/String/toUpperCase)

Nauka konwertowania stringa na małe litery jest niezbędna w programowaniu w języku Javascript. Dzięki temu artykułowi już teraz możesz zacząć stosować najprostszą metodę `toLowerCase()` w swoim kodzie. Nie zapomnij również wypróbować innych sposobów konwersji stringa!