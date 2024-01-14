---
title:    "TypeScript: Konwersja daty na ciąg znaków"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwersja daty na łańcuch znaków jest niezbędnym elementem w wielu projektach TypeScript. Pozwala na wyświetlenie daty w czytelny sposób lub na przechowywanie jej w bazie danych. Dowiedz się, jak łatwo przeprowadzić tę konwersję w TypeScript!

## Jak to zrobić

```TypeScript
const currentDate = new Date(); // tworzy obiekt daty z aktualną datą i czasem
const stringDate = currentDate.toDateString(); // konwertuje datę na łańcuch znaków w postaci pełnej daty
console.log(stringDate); // wyświetla "Wed Oct 13 2021"
```

Innym sposobem na konwersję daty na łańcuch znaków jest użycie funkcji `toLocaleString()` wraz z odpowiednimi opcjami. Na przykład:

```TypeScript
const currentDate = new Date(); // tworzy obiekt daty z aktualną datą i czasem
const options = { weekday: 'long', year: 'numeric', month: 'long', day: 'numeric' }; // ustawia opcje wyświetlania daty
const stringDate = currentDate.toLocaleString('pl-PL', options); // konwertuje datę na łańcuch znaków w postaci pełnej daty w polskim formacie
console.log(stringDate); // wyświetla "środa, 13 października 2021"
```

## Deep Dive

Przyjrzyjmy się bliżej funkcjom `toDateString()` i `toLocaleString()` używanym w powyższych przykładach:

- `toDateString()` zwraca łańcuch znaków w formacie według normy ISO 8601, czyli "RRRR-MM-DD".
- `toLocaleString()` można dostosować do różnych języków i regionów dzięki opcji `locale`. Opcja `options` pozwala na wybór formatu wyświetlania daty oraz czasu, w zależności od preferencji.

Na przykład, używając funkcji `toLocaleString()` z opcją `locale` równą "en-US", otrzymamy datę w formacie amerykańskim, czyli "MM/DD/RRRR".

## Zobacz także

- [Dokumentacja Date w TypeScript](https://www.typescriptlang.org/docs/handbook/basic-types.html#date)
- [Instrukcja łańcuchów znaków w TypeScript](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)