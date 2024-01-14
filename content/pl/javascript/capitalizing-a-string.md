---
title:    "Javascript: Zamiana na wielkie litery w ciągu znaków"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego warto używać JavaScript do zmiany wielkości liter w zmiennych?

W programowaniu często zdarza się sytuacja, w której konieczne jest zmienienie wielkości liter w zmiennej. Na przykład, powinno się uniknąć błędów związanych z nieprawidłowym formatowaniem tekstu lub chce się podkreślić pewną część zdania. W takiej sytuacji stosuje się funkcję, która zmienia większe litery na mniejsze lub na odwrót. W tym blogu omówimy, jak w prosty sposób zaimplementować tę funkcję w języku JavaScript.

## Jak to zrobić?

Aby zmienić wielkość liter w zmiennej, używamy funkcji "toUpperCase()" i "toLowerCase()". Obie te funkcje są wbudowane w język JavaScript i służą do zmiany liter na wielkie i małe odpowiednio. Przeanalizujmy poniższy kod:

```javascript
let text = "Witaj na Blogu!";
console.log(text.toUpperCase());
console.log(text.toLowerCase());
```

W powyższym przykładzie tworzymy zmienną "text" z tekstem "Witaj na Blogu!". Następnie, używając funkcji "toUpperCase()" zmieniamy wszystkie litery na wielkie. W drugim przypadku, przy użyciu "toLowerCase()" zmieniamy wszystkie litery na małe. W obu przypadkach, otrzymamy następujący wynik:

```
WITAJ NA BLOGU!
witaj na blogu!
```

Jeśli chcemy zmienić tylko pierwszą literę w zdaniu na wielką, możemy użyć metody "charAt()" i połączyć ją z "toUpperCase()". Zobaczmy to na przykładzie:

```javascript
let name = "anna";
let capitalizedName = name.charAt(0).toUpperCase() + name.slice(1);
console.log(capitalizedName);
```

W powyższym przykładzie, tworzymy zmienną "name" z tekstem "anna". Następnie, używając metody "charAt()" pobieramy pierwszą literę (indeks 0) i zmieniamy ją na wielką za pomocą "toUpperCase()". Następnie, łączymy tę pierwszą literę z resztą tekstu za pomocą metody "slice(1)", która pobiera część tekstu od drugiego znaku do końca. W ten sposób, otrzymujemy zmieniony tekst, który wyświetlamy za pomocą funkcji "console.log()".

```
Anna
```

## Głębsza analiza

W JavaScript, zmienne przechowują wszystkie rodzaje danych, takie jak liczby, stringi lub obiekty. Kiedy używamy funkcji do zmiany wielkości liter, nie modyfikujemy ich w miejscu. Zamiast tego, tworzymy nową wartość i przypisujemy ją do zmiennej. Jest to ważne, ponieważ zmienne są traktowane jako niezmiennikowalne (immutable), co oznacza, że nie można zmienić ich wartości bezpośrednio.

W przypadku, gdy nie chcemy tworzyć nowej wartości, ale zmienić aktualną zmienną, możemy użyć metody "replace()" i wyrażenia regularnego (regular expression). Wyrażenia regularne to wzorce służące do identyfikowania i manipulowania tekstu. Na przykład, jeśli chcielibyśmy zamienić wszystkie spacje na myślniki w tekście, możemy to zrobić za pomocą poniższego kodu:

```javascript
let text = "To jest przykładowy tekst";
let updatedText = text.replace(/\s/g, "-");
console.log(updatedText);
```

W powyższym przykładzie, używamy metody "replace()" i wyrażenia regularnego "/\s/g", które oznacza "wszystkie znaki białe" i zastępujemy je myślnikami "-". Dzięki tem