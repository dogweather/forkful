---
title:                "Zamiana liter na wielkie w łańcuchu znaków"
aliases: - /pl/javascript/capitalizing-a-string.md
date:                  2024-02-03T19:05:48.151744-07:00
model:                 gpt-4-0125-preview
simple_title:         "Zamiana liter na wielkie w łańcuchu znaków"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?
Wielką literą na początku ciągu znaków (ang. capitalizing a string) rozumiemy zamianę pierwszej litery ciągu na dużą, pozostawiając pozostałe znaki bez zmian. Operacja ta jest często wykonywana w JavaScript w celu formatowania danych wejściowych użytkownika, wyświetlania nazw lub tytułów oraz zapewnienia spójności tekstów interfejsu użytkownika.

## Jak to zrobić:
W JavaScript nie ma wbudowanej metody bezpośrednio przeznaczonej do zmiany pierwszej litery ciągu na wielką, ale można to łatwo zrealizować, korzystając z podstawowych metod manipulacji ciągami.

### Używając standardowego JavaScript
```javascript
function capitalize(str) {
  if (!str) return '';
  return str.charAt(0).toUpperCase() + str.slice(1);
}

console.log(capitalize('hello world')); // Wynik: "Hello world"
```

### Wersja ES6
Z wykorzystaniem literałów szablonowych z ES6, funkcję można zapisać w bardziej zwięzły sposób:
```javascript
const capitalize = (str) => !str ? '' : `${str[0].toUpperCase()}${str.slice(1)}`;

console.log(capitalize('hello ES6')); // Wynik: "Hello ES6"
```

### Używając Lodash
Lodash to popularna biblioteka pomocnicza osób trzecich, oferująca szeroki zakres funkcji do manipulacji wartościami JavaScript, w tym ciągami znaków. Aby uczynić pierwszą literę ciągu wielką za pomocą Lodash:
```javascript
// Najpierw zainstaluj lodash, jeśli jeszcze tego nie zrobiłeś: npm install lodash
const _ = require('lodash');

console.log(_.capitalize('LODASH example')); // Wynik: "Lodash example"
```
_Zwróć uwagę, że Lodash nie tylko zamienia pierwszą literę na wielką, ale również konwertuje resztę ciągu na małe litery, co różni się nieco od prostej implementacji w JavaScript._

### Używając CSS (Tylko do celów wyświetlania)
Jeśli celem jest uczynienie tekstu z wielką literą na początku do wyświetlenia w interfejsie użytkownika, można użyć CSS:
```css
.capitalize {
  text-transform: capitalize;
}
```
```html
<div class="capitalize">hello css</div> <!-- Wyświetla się jako "Hello css" -->
```
**Uwaga:** Ta metoda zmienia sposób wyświetlania tekstu na stronie internetowej, bez zmiany samego ciągu znaków w JavaScript.
