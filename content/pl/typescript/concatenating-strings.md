---
title:                "Łączenie ciągów znaków"
html_title:           "TypeScript: Łączenie ciągów znaków"
simple_title:         "Łączenie ciągów znaków"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

Zamiana, łączenie lub łączenie ciągów jest powszechną operacją w programowaniu. W przypadku, gdy trzeba wyświetlić użytkownikowi tekstowy komunikat, podać dokładną ścieżkę pliku lub stworzyć skomplikowany adres URL, konieczne jest połączenie kilku ciągów znaków w jeden. W tym artykule dowiesz się, jak w łatwy sposób wykonać tę operację w języku TypeScript.

## Jak to zrobić

Aby połączyć dwa ciągi znaków w języku TypeScript, możesz skorzystać z operatora "+" lub metody "concat()". Przykładowy kod wykorzystujący obie metody wyglądałby następująco:

```TypeScript
// Operator "+"
let firstName = "Anna";
let lastName = "Kowalska";

let fullName = firstName + " " + lastName;
console.log(fullName); // Wynik: Anna Kowalska

// Metoda "concat()"
let firstWord = "Hello";
let secondWord = "World";

let sentence = firstWord.concat(" ", secondWord);
console.log(sentence); // Wynik: Hello World
```

Jednym z większych wyzwań, jeśli chodzi o łączenie ciągów znaków, jest kontrola ich ilości i formatu. Dlatego warto poznać inne metody, które umożliwią nam bardziej zaawansowane operacje. Przykładowo, jeśli chcesz dodać jakieś określenie lub wartość do ciągu, możesz skorzystać z metody "replace()" lub "trim()". Sprawdź poniższy kod:

```TypeScript
let sentence = "Mam 5 dolarów";
let valueToAdd = 10;

let newValue = sentence.replace("5", valueToAdd.toString());
console.log(newValue); // Wynik: Mam 10 dolarów

let text = "   Cześć,   ";
let trimmedText = text.trim();
console.log(trimmedText); // Wynik: "Cześć"
```

## Głębsze zagadnienia

Warto zauważyć, że w języku TypeScript każdy ciąg jest traktowany jako obiekt typu String. Dzięki temu możemy korzystać z wielu metod dostępnych dla typów obiektowych, np. "toUpperCase()" czy "substring()". Poniżej przedstawione jest przykładowe użycie tych metod:

```TypeScript
let text = "Ważna informacja!";
console.log(text.toUpperCase()); // Wynik: WAŻNA INFORMACJA!
console.log(text.substring(0, 6)); // Wynik: Ważna
```

Kolejnym ważnym aspektem jest to, że łączenie ciągów znaków jest procesem kosztownym dla pamięci i czasu wykonania. Dlatego ważne jest, aby unikać zbędnych i powtarzalnych operacji łączenia w kodzie. Możliwe jest również wykorzystanie szablonów literałowych, które pozwalają na wprowadzenie dynamicznych wartości do ciągu znaków w bardziej czytelny sposób. Przykład:

```TypeScript
let productName = "Smartfon";
let unitPrice = 1000;
let quantity = 2;
let totalPrice = `Kupiłeś ${productName} w cenie ${unitPrice} złotych. Całkowita kwota do zapłaty to ${unitPrice * quantity} złotych.`;
console.log(totalPrice); // Wynik: Kupiłeś Smartfon w cenie 1000 złotych. Całkowita kwota do zapłaty to 2000 złotych.
```

## Zobacz także

- [Dokumentacja języka TypeScript](https://www.typescriptlang.org/docs/)
- [Oficjalna strona TypeScript](https://www.typescriptlang.org/)
- [Inne artykuły na temat programowania w języku TypeScript](https://dev.to/search?q=typescript)