---
title:                "TypeScript: Porównywanie dwóch dat"
programming_language: "TypeScript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego
Programowanie to dziedzina, która stale rozwija się i ewoluuje, a wraz z nią pojawiają się coraz to nowsze i bardziej zaawansowane zadania do wykonania. Jednym z takich zadań jest porównywanie dwóch dat. Możesz być zaskoczony, ale w programowaniu często potrzebowane jest porównywanie dat w celu ustalenia, która jest wcześniejsza lub późniejsza. Dlatego jest to ważna umiejętność, którą warto opanować.

## Jak to zrobić
W celu porównania dwóch dat w TypeScript możemy skorzystać z obiektu `Date`. Poniżej przedstawiamy przykładowy kod oraz jego wynik w blokach kodu markdown z oznaczeniem języka TypeScript:

```TypeScript
// Ustalenie dwóch dat
const firstDate = new Date("2020-01-01"); 
const secondDate = new Date("2021-01-01"); 

// Porównanie dat przy pomocy operatorów porównania
console.log(firstDate < secondDate); // Output: true
console.log(firstDate > secondDate); // Output: false

// Porównanie dat przy pomocy metody getTime()
console.log(firstDate.getTime() < secondDate.getTime()); // Output: true
console.log(firstDate.getTime() > secondDate.getTime()); // Output: false
```

Jak widać, istnieje kilka sposobów na porównanie dat w TypeScript. W pierwszym przypadku możemy skorzystać z operatorów porównania, które porównują wartości dat. Natomiast w drugim przypadku wykorzystujemy metodę `getTime()`, która zwraca wartość daty w milisekundach.

## Głębsza analiza
Warto również zaznajomić się z innymi metodami dostępnymi w obiekcie `Date`, które mogą okazać się użyteczne podczas porównywania dat. Należą do nich między innymi `getFullYear()` - zwracająca rok daty, `getMonth()` - zwracająca miesiąc daty (0 - styczeń, 11 - grudzień), czy `getDate()` - zwracająca dzień miesiąca z daty.

Ponadto, warto pamiętać o możliwości konwersji dat do innych formatów, takich jak string czy liczba, przy pomocy odpowiednich metod jak `toString()` czy `valueOf()`. Ważne jest również uwzględnienie różnic w strefach czasowych i przeliczenie ich na tę samą przed porównaniem dat.

## Zobacz również
Jeśli chcesz poszerzyć swoją wiedzę na temat porównywania dat w TypeScript, polecamy zapoznanie się z poniższymi linkami:

- Oficjalna dokumentacja TypeScript dotycząca obiektu `Date` (https://www.typescriptlang.org/docs/handbook/utilities.html#date)
- Artykuł na stronie Medium pt. "Working with Dates in TypeScript" autorstwa Bogdana Nedelkovskiego (https://medium.com/@bogdanlz/working-with-dates-in-typescript-c40d25eac4f4)