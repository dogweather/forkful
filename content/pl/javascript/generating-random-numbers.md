---
title:                "Generowanie losowych liczb"
html_title:           "Javascript: Generowanie losowych liczb"
simple_title:         "Generowanie losowych liczb"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego

Generowanie liczb losowych jest ważnym elementem w wielu dziedzinach programowania, takich jak symulacje, gry lub testowanie oprogramowania. Umiejętność generowania losowych wartości jest niezbędna przy tworzeniu aplikacji, które wymagają losowego wyboru lub tworzenia danych.

## Jak to zrobić

```Javascript
// Generowanie losowej liczby całkowitej z zakresu 1-10
let randomNumber = Math.floor(Math.random() * 10) + 1;
console.log(randomNumber); // Wartość mogąca się zmienić w każdej iteracji

// Wybór losowego elementu z tablicy
let fruits = ["jabłko", "banan", "truskawka", "pomarańcza", "kiwi"];
let randomFruit = fruits[Math.floor(Math.random() * fruits.length)];
console.log(randomFruit); // Wartość mogąca się zmienić w każdej iteracji
```

Kod wykorzystuje wbudowaną funkcję `Math.random()`, która zwraca liczbę pseudolosową z przedziału `0-1`. Aby ograniczyć zakres liczb lub wybrać losowy element z tablicy, należy odpowiednio pomnożyć i zaokrąglić wynik. W ten sposób uzyskujemy różnorodne, nieprzewidywalne wartości.

## Deep Dive

Aby jeszcze lepiej zrozumieć, jak działa generowanie liczb losowych, warto poznać kilka podstawowych pojęć. Po pierwsze, trzeba zauważyć, że liczby losowe wygenerowane przez komputer są w rzeczywistości pseudolosowe, czyli wytwarzane za pomocą algorytmu, który nie jest w 100% losowy. Jednakże, dzięki skomplikowanym wzorom matematycznym, wyniki te wyglądają na całkowicie losowe.

Kolejnym ważnym pojęciem jest ziarno (ang. seed) - wartość, która determinuje początkowy stan generatora liczb losowych. Jeśli nie podamy ziarna, komputer wykorzysta aktualny czas jako ziarno i tym samym zapewni większą zmienność liczb. Jednakże, jeśli podamy stałe ziarno, wówczas będziemy zawsze otrzymywać te same wyniki.

Istnieją też zaawansowane techniki generowania liczb losowych, takie jak metoda Monte Carlo, która wykorzystuje szereg prób losowych do obliczenia prawdopodobieństwa danej sytuacji.

## Zobacz też

- [Math.random() - dokumentacja MDN](https://developer.mozilla.org/pl/docs/Web/JavaScript/Referencje/Obiekty/Globalne/Math/random)
- [Pseudolosowość - Wikipedia](https://pl.wikipedia.org/wiki/Pseudolosowo%C5%9B%C4%87)
- [Algorithms for Generating Random Objects - artykuł](https://codeburst.io/algorithms-for-generating-random-objects-c0ac65af458c)