---
date: 2024-01-26 04:42:27.641406-07:00
description: "Jak to zrobi\u0107: JavaScript nie posiada wbudowanego wsparcia dla\
  \ liczb zespolonych, ale mo\u017Cna podwin\u0105\u0107 r\u0119kawy i poradzi\u0107\
  \ sobie z tym za pomoc\u0105 obiekt\xF3w i\u2026"
lastmod: '2024-03-13T22:44:35.790445-06:00'
model: gpt-4-0125-preview
summary: "JavaScript nie posiada wbudowanego wsparcia dla liczb zespolonych, ale mo\u017C\
  na podwin\u0105\u0107 r\u0119kawy i poradzi\u0107 sobie z tym za pomoc\u0105 obiekt\xF3\
  w i matematyki."
title: Praca z liczbami zespolonymi
weight: 14
---

## Jak to zrobić:
JavaScript nie posiada wbudowanego wsparcia dla liczb zespolonych, ale można podwinąć rękawy i poradzić sobie z tym za pomocą obiektów i matematyki. Oto szybki przegląd.

```javascript
class ComplexNumber {
  constructor(real, imaginary) {
    this.real = real;
    this.imaginary = imaginary;
  }

  add(other) {
    return new ComplexNumber(this.real + other.real, this.imaginary + other.imaginary);
  }

  // ...dodaj więcej metod (odejmowanie, mnożenie, dzielenie) według potrzeb

  toString() {
    return `${this.real} + ${this.imaginary}i`;
  }
}

const a = new ComplexNumber(1, 2);
const b = new ComplexNumber(3, 4);
const wynik = a.add(b);

console.log(`Wynik: ${wynik}`); // Wynik: 4 + 6i
```

## Pogłębiona analiza
Liczby zespolone istnieją od XVI wieku, dzięki włoskiemu matematykowi Gerolamo Cardano. Stały się kluczowe w różnych dziedzinach, takich jak inżynieria i fizyka. We współczesnym programowaniu są kluczowe dla symulacji i algorytmów wymagających wielowymiarowości.

Teraz, JavaScript nie jest zaopatrzony w liczby zespolone natywnie. Ale oprócz opcji DIY, można użyć bibliotek matematycznych takich jak math.js lub numeric.js. Zapewniają one moc do cięższej pracy z liczbami zespolonymi, dodając takie korzyści jak więcej operacji, obliczanie magnitudy i znajdowanie argumentu.

Pod maską, kiedy operujesz na liczbach zespolonych, to jakbyś zarządzał dwoma osobnymi liczbami, które są ze sobą związane. Dodawanie i odejmowanie to prosta gra – dopasowujesz rzeczywiste do rzeczywistych, urojone do urojonych. Mnożenie i dzielenie stają się bardziej pikantne z tańcem międzywyrazowym i wymagają większej uwagi.

## Zobacz również
- Dokumentacja MDN Web Docs na temat JavaScript: https://developer.mozilla.org/en-US/docs/Web/JavaScript/A_re-introduction_to_JavaScript
- Math.js, biblioteka matematyczna zawierająca liczby zespolone: https://mathjs.org/docs/datatypes/complex_numbers.html
- Numeric.js, inna biblioteka: http://numericjs.com/documentation.html
- Głębsze spojrzenie na liczby zespolone (skupione na matematyce): https://mathworld.wolfram.com/ComplexNumber.html
