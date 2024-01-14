---
title:                "TypeScript: Ekstrakcja podciągów"
simple_title:         "Ekstrakcja podciągów"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszym poście porozmawiamy o wydobywaniu podłańcuchów w języku TypeScript. Jest to bardzo przydatna umiejętność, która pozwala na wybieranie i przetwarzanie konkretnych części tekstu. Czy kiedykolwiek zastanawiałeś się, jak programy potrafią wyciągnąć z tekstu takie informacje jak numer telefonu czy adres email? Okazuje się, że odpowiedzią na to pytanie jest wykorzystywanie funkcji do wydobywania podłańcuchów.

## Jak to zrobić

Aby wydobywać podłańcuchy w języku TypeScript, należy użyć funkcji `substring()` lub `slice()`. Obie te metody pozwalają na wybieranie określonej części stringa na podstawie indeksów. Przykładowo, jeśli mamy tekst "Hello World!", to chcąc wydobyć tylko słowo "World", możemy użyć funkcji `substring(6,11)`, gdzie pierwszy argument oznacza indeks początkowy, a drugi indeks końcowy (indeksy są numerowane od zera). Możemy również wykorzystać funkcję `slice()` w podobny sposób.

```TypeScript
const text: string = "Hello World!";
const substring: string = text.substring(6,11);
console.log(substring); // Output: World
```

Możemy również wykorzystać te funkcje do wydobywania podłańcuchów zmiennych przechowujących informacje, takich jak numery telefonów czy adresy email.

```TypeScript
const phoneNumber: string = "+48 123 456 789";
const extractedNumber: string = phoneNumber.slice(4,16);
console.log(extractedNumber); // Output: 123 456 789

const email: string = "example@mail.com";
const extractedDomain: string = email.substring(8,14);
console.log(extractedDomain); // Output: mail
```

## Deep Dive

Funkcje `substring()` i `slice()` mają kilka różnic, które może być warto poznać. Pierwsza z nich dotyczy sposobu określenia indeksów. W przypadku funkcji `substring()` podanie indeksu końcowego nie jest wymagane, ponieważ jeśli go nie podamy, zostanie pobrana cała część aranżenu stringa od podanego indeksu początkowego do samego końca. Natomiast funkcja `slice()` wymaga podania indeksu końcowego, jednakże możemy podać ujemną wartość, co spowoduje, że indeks będzie odliczany od końca stringa. Przykładowo, ujemny indeks `-5` oznacza "5 pozycji do końca".

Druga różnica dotyczy sposobu obsługi ujemnych indeksów. W przypadku `substring()` ujemny indeks jest zamieniany na 0, więc oznacza on po prostu pierwszą pozycję w stringu. Natomiast w `slice()` ujemny indeks jest traktowany jako `string.length + ujemny indeks`, czyli na przykład `-5` w stringu "Hello" będzie oznaczać indeks 0.

## Zobacz też

- Dokumentacja funkcji `substring()`: https://www.typescriptlang.org/docs/handbook/strings.html#substring
- Dokumentacja funkcji `slice()`: https://www.typescriptlang.org/docs/handbook/strings.html#slice
- Przykładowe zadania z wykorzystaniem extractSubstring: https://www.typescriptlang.org/docs/handbook/strings.html#coding-examples-substring

Dzięki wykorzystaniu funkcji `substring()` i `slice()` możemy łatwo i szybko wydobywać interesujące nas podłańcuchy z tekstu. Będzie to bardzo przydatne w różnego rodzaju programach, gdzie potrzebujemy przetwarzać i analizować tekst w celu wyłuskania konkretnych informacji. Mam nadzieję, że ten wpis był