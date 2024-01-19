---
title:                "Generowanie liczb losowych"
html_title:           "Gleam: Generowanie liczb losowych"
simple_title:         "Generowanie liczb losowych"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Co to jest i Dlaczego?

Generowanie losowych liczb w programowaniu to proces tworzenia liczb, które nie są przewidywalne. Programiści to robią, kiedy chcą dodać nieprzewidywalność do swojego kodu, taką jak losowy wybór elementu z listy.

## Jak to zrobić:

Oto przykładowy kod pokazujący jak możesz generować losową liczbę w JavaScript:

```Javascript
function losowaLiczba() {
  return Math.random();
}
console.log(losowaLiczba());
```
 
I oto przykładowy rezultat:
 ```Javascript
0.7151308377552927
```

## W Głąb Tematu:

Generowanie losowych liczb zaczęło się od potrzeby symulacji zjawisk naturalnych w komputerach. To było potrzebne np. dla naukowców modelujących zjawiska atmosferyczne.

Alternatywą generowania liczb w JavaScript jest korzystanie z obiektu `crypto.getRandomValues` lub z zewnętrznych bibliotek jak Mersenne Twister.

Detail implementacji funkcji `Math.random` jest trochę skomplikowany - nie jest to prawdziwie losowe, ale powinien być wystarczająco losowy dla większości zastosowań.

## Zobacz również:

1. [MDN Web Docs: Math.random()](https://developer.mozilla.org/pl/docs/Web/JavaScript/Referencje/Obiekty/Math/random)
2. [Wikipedia: Generator liczb pseudolosowych](https://pl.wikipedia.org/wiki/Generator_liczb_pseudolosowych)
3. [npm: Mersenne Twister package](https://www.npmjs.com/package/mersenne-twister)