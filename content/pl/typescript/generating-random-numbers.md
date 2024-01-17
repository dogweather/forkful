---
title:                "Generowanie losowych liczb"
html_title:           "TypeScript: Generowanie losowych liczb"
simple_title:         "Generowanie losowych liczb"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Czym jest generowanie losowych liczb i dlaczego jest to ważne?
Generowanie losowych liczb jest jedną z podstawowych umiejętności każdego programisty. Jest to proces, w którym program generuje liczby w sposób losowy, czyli bez określonego wzorca lub kolejności. Programiści często używają tej funkcji do zapewnienia różnorodności i losowości w swoich projektach, na przykład przy tworzeniu gier lub symulacji.

## Jak to zrobić?
W TypeScript generowanie losowych liczb może być wykonane za pomocą funkcji Math.random(). Przykładowo, jeśli chcesz wygenerować losową liczbę z przedziału 1 do 10, możesz użyć kodu:

```TypeScript
let randomNumber = Math.floor(Math.random() * 10) + 1;
console.log(randomNumber);
```

Kod ten najpierw korzysta z funkcji Math.random() do wygenerowania liczby z przedziału od 0 do 1, a następnie mnoży ją przez 10 (liczbę liczb w przedziale) i dodaje 1, aby wynik był z przedziału od 1 do 10. Wówczas używamy funkcji Math.floor() do zaokrąglenia liczby do najbliższej całkowitej i przypisujemy ją do zmiennej randomNumber. W drugim wierszu kodu wyświetlamy wygenerowaną liczbę w konsoli.

## Głębsze zagadnienia
Generowanie losowych liczb jest ważnym elementem programowania już od lat. Początkowo programiści używali do tego celu tzw. generatorów pseudolosowych, czyli funkcji, które generowały liczby wydające się losowe, ale w rzeczywistości były wyznaczone przez określony wzór. Jednak dzięki postępowi technologicznemu i dostępności nowych algorytmów, obecnie istnieją bardziej zaawansowane metody generowania prawdziwie losowych liczb.

Alternatywnie, jeśli z jakiegoś powodu nie chcesz używać funkcji Math.random() w swoim projekcie, istnieją inne biblioteki i narzędzia dostępne dla TypeScript, takie jak Seedrandom lub Random.js.

Jeśli jesteś ciekawy, jak funkcja Math.random() jest zaimplementowana, warto przeczytać dokumentację lub przejrzeć kod źródłowy TypeScript lub JavaScript.

## Zobacz także
- Dokumentacja JavaScript na temat funkcji Math.random(): https://developer.mozilla.org/pl/docs/Web/JavaScript/Reference/Global_Objects/Math/random
- Biblioteka Seedrandom do generowania losowych liczb w TypeScript: https://www.npmjs.com/package/seedrandom
- Narzędzie Random.js do generowania losowych liczb w TypeScript: https://github.com/duhaime/random_js