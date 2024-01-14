---
title:    "TypeScript: Obliczanie daty w przyszłości lub przeszłości"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Dlaczego

Czasami w programowaniu potrzebujemy obliczyć datę w przyszłości lub przeszłości, na przykład do wyświetlenia harmonogramu lub do obliczenia daty ważności. W tym artykule dowiesz się, jak możesz łatwo to zrobić w języku TypeScript.

## Jak to zrobić

Poniżej znajdują się dwa przykładowe kody, które pokażą Ci, jak obliczyć datę w przyszłości i przeszłości w TypeScript:

### Obliczanie daty w przyszłości

```TypeScript
const dzisiaj = new Date();
const dniDoDodania = 14;
const dataPrzyszla = new Date(dzisiaj.getTime() + dniDoDodania * 24 * 60 * 60 * 1000);

console.log(`Data za dwa tygodnie: ${dataPrzyszla.toDateString()}`);
```

W tym przykładzie tworzymy obiekt daty dla dzisiejszego dnia, a następnie dodajemy do niego liczbę dni, jaką chcemy przesunąć datę w przyszłość. Przy użyciu metody `getTime()` uzyskujemy liczbę milisekund reprezentujących datę dzisiejszą, a następnie mnożymy przez 24, 60, 60, 1000, aby przekonwertować dzień na milisekundy. Następnie tworzymy nowy obiekt daty na podstawie tej liczby i wyświetlamy go w konsoli.

### Obliczanie daty w przeszłości

```TypeScript
const dzisiaj = new Date();
const dniDoOdejmowania = 7;
const dataPrzeszla = new Date(dzisiaj.getTime() - dniDoOdejmowania * 24 * 60 * 60 * 1000);

console.log(`Data sprzed tygodnia: ${dataPrzeszla.toDateString()}`);
```

Podobnie jak w poprzednim przykładzie, tworzymy obiekt daty dla dzisiejszego dnia. Tym razem jednak odejmujemy od niego liczbę dni, aby przesunąć datę w przeszłość. Następnie tworzymy nowy obiekt daty i wyświetlamy go w konsoli.

## Deep Dive

Obliczanie daty w przyszłości lub przeszłości może wymagać nieco większej uwagi, ponieważ należy uwzględnić różnice w długościach miesięcy i lat. W przypadku prostych przykładów, jakie przedstawiliśmy powyżej, nie jest to problemem. Jednak w bardziej skomplikowanych przypadkach, na przykład gdy chcemy dodać/odjąć lata lub miesiące, powinniśmy użyć metody `setFullYear()` lub `setMonth()`.

Na przykład, jeśli chcemy dodać 6 miesięcy do bieżącej daty, zamiast mnożyć przez liczbę milisekund w roku, możemy po prostu użyć metody `setMonth()`:

```TypeScript
const dzisiaj = new Date();
const miesiaceDoDodania = 6;

dzisiaj.setMonth(dzisiaj.getMonth() + miesiaceDoDodania);

console.log(`Data za 6 miesięcy: ${dzisiaj.toDateString()}`);
```

Używając tej metody musimy jednak pamiętać o tym, że numery miesięcy są indeksowane od 0, więc styczeń to 0, luty to 1, itd. Dlatego też, aby dodać 6 miesięcy, używamy metody `getMonth()` do pobrania aktualnego miesiąca i dodajemy do niego 6.

## Zobacz także

Jeśli jesteś zainteresowany bardziej zaawansowanym obliczaniem dat w TypeScript, polecamy zapoznanie się z dokumentacją, która może być dla Ciebie pomocna:

- [Dokumentacja Date w TypeScript](https://www.typescriptlang.org