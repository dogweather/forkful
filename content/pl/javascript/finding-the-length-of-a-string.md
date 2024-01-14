---
title:                "Javascript: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Dlaczego powinieneś/musisz znaleźć długość łańcucha znaków w swoim kodzie Javascript? Jest to niezbędne do wielu operacji, takich jak przycinanie, łączenie lub porównywanie różnych łańcuchów. Dlatego znajomość tej umiejętności jest nie tylko przydatna, ale również niezbędna w codziennej pracy programisty.

## Jak to zrobić

Aby znaleźć długość łańcucha znaków, musimy użyć wbudowanej funkcji `length`. Przykładowy kod wyglądałby następująco:

```JavaScript
let str = "Witaj, ten tekst zawiera 24 znaki";
console.log(str.length);
```

W tym przypadku, wynikiem będzie liczba 24, ponieważ funkcja `length` zwraca liczbę znaków w danym łańcuchu. Możesz również użyć tej funkcji wraz z innymi metodami, takimi jak `substring`, aby uzyskać długość określonego fragmentu tekstu.

```JavaScript
let str = "Witaj, ten tekst zawiera 24 znaki";
let substring = str.substring(0,5); //pierwsze 5 znaków
console.log(substring.length); //wynikiem będzie 5
```

## W głąb tematu

Podczas wyznaczania długości łańcucha, należy pamiętać, że każdy znak ma przypisaną wartość Unicode. W związku z tym, jeśli używasz znaków, które nie znajdują się w standardowym alfabecie, takich jak polskie znaki, wynik funkcji `length` może różnić się od oczekiwanego.

Innym ważnym aspektem jest fakt, że funkcja `length` nie uwzględnia białych spacji na końcu łańcucha. Oznacza to, że jeśli dodałbyś spację na końcu swojego tekstu, długość będzie o 1 mniejsza niż faktyczna liczba znaków.

## Zobacz również

- [Dokumentacja Javascript o funkcji length](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [Inne wbudowane funkcje Javascript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String#Methods)
- [Artykuł o znakach Unicode](https://en.wikipedia.org/wiki/Unicode)