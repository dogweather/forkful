---
title:                "TypeScript: Otrzymywanie aktualnej daty"
simple_title:         "Otrzymywanie aktualnej daty"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego

Pozyskiwanie aktualnej daty jest nieodłączną częścią wielu aplikacji i skryptów. W TypeScript możemy to zrobić w prosty sposób, korzystając z wbudowanej funkcji "Date()".

## Jak to zrobić

Możemy uzyskać aktualną datę na kilka sposobów, na przykład:

```TypeScript
let currentDate = new Date();
console.log(currentDate);
```

Wynik wyświetli się na konsoli w formacie: `YYYY-MM-DD HH:MM:SS`.

Możemy również wyświetlić separowane dane z daty, takie jak dzień, miesiąc czy rok:

```TypeScript
let currentDate = new Date();

let day = currentDate.getDate(); // pobieramy dzień
let month = currentDate.getMonth() + 1; // pobieramy miesiąc (+ 1, ponieważ styczeń to index 0)
let year = currentDate.getFullYear(); // pobieramy rok

console.log(`${day}.${month}.${year}`); // wyświetlamy date w formacie DD.MM.YYYY
```

Wynik: `13.12.2021`

## Deep Dive

Funkcja "Date()" może przyjmować również argumenty, pozwalając nam na ustawianie własnej daty. Możemy podać rok, miesiąc i dzień, aby uzyskać konkretny dzień tygodnia, na przykład:

```TypeScript
let specifiedDate = new Date(2021, 11, 25); // rok, miesiąc, dzień
console.log(specifiedDate.getDay()); // wyświetli index dnia tygodnia (0 - niedziela, 6 - sobota)
```

Wynik: `6`

Funkcja "Date()" pozwala również na uzyskanie innych danych, takich jak godzina, minuty czy sekundy. Dokładny opis dostępnych metod można znaleźć w dokumentacji TypeScript.

## Zobacz również

- Dokumentacja TypeScript dotycząca funkcji "Date()": https://www.typescriptlang.org/docs/handbook/standard-library.html#date
- Przykładowe tutorial na temat pozyskiwania daty w TypeScript: https://www.techiedelight.com/get-current-date-time-timestamp-typescript/