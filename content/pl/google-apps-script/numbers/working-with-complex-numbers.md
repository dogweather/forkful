---
title:                "Praca z liczbami zespolonymi"
date:                  2024-02-01T22:07:47.902867-07:00
model:                 gpt-4-0125-preview
simple_title:         "Praca z liczbami zespolonymi"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/google-apps-script/working-with-complex-numbers.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co & Dlaczego?
Liczby zespolone, przedstawiane jako kombinacja jednostek rzeczywistych i urojonych (np. 3 + 4i), są fundamentalne w różnych problemach obliczeniowych, zwłaszcza w inżynierii, fizyce i matematyce stosowanej. Nauka manipulowania tymi liczbami w Google Apps Script pozwala programistom rozszerzyć ich możliwości na obliczenia naukowe, przetwarzanie sygnałów i dalej.

## Jak:
Google Apps Script nie ma wbudowanego wsparcia dla liczb zespolonych, co wymusza implementację własnej funkcjonalności. Poniżej znajduje się podstawowa struktura do obsługi liczb zespolonych, w tym dodawanie, odejmowanie i mnożenie.

```javascript
// Zdefiniuj konstruktor dla liczb zespolonych
function Complex(real, imag) {
  this.real = real;
  this.imag = imag;
}

// Metoda do dodawania dwóch liczb zespolonych
Complex.prototype.add = function(other) {
  return new Complex(this.real + other.real, this.imag + other.imag);
};

// Metoda do odejmowania dwóch liczb zespolonych
Complex.prototype.subtract = function(other) {
  return new Complex(this.real - other.real, this.imag - other.imag);
};

// Metoda do mnożenia dwóch liczb zespolonych
Complex.prototype.multiply = function(other) {
  return new Complex(
    this.real * other.real - this.imag * other.imag,
    this.real * other.imag + this.imag * other.real
  );
};

// Przykładowe użycie
var num1 = new Complex(3, 4);
var num2 = new Complex(1, 2);

// Dodawanie dwóch liczb zespolonych
var sum = num1.add(num2);
console.log(`Suma: ${sum.real} + ${sum.imag}i`); // Suma: 4 + 6i

// Odejmowanie dwóch liczb zespolonych
var difference = num1.subtract(num2);
console.log(`Różnica: ${difference.real} + ${difference.imag}i`); // Różnica: 2 + 2i

// Mnożenie dwóch liczb zespolonych
var product = num1.multiply(num2);
console.log(`Iloczyn: ${product.real} + ${product.imag}i`); // Iloczyn: -5 + 10i
```

## W głąb tematu:
Koncepcja liczb zespolonych sięga XVI wieku, ale to prace matematyków takich jak Euler i Gauss umocniły ich miejsce w matematyce. Pomimo ich użyteczności, liczby zespolone nie są bezpośrednio obsługiwane w JavaScript lub, przez rozszerzenie, w Google Apps Script. Brak natywnego wsparcia oznacza, że operacje na liczbach zespolonych muszą być ręcznie implementowane, jak pokazano. Chociaż zapewnia to dobrą okazję do nauki i wystarczającą funkcjonalność dla podstawowych potrzeb, do ciężkich obliczeń wymagających liczb zespolonych, warto rozważyć wykorzystanie innych środowisk programistycznych bardziej przystosowanych do obliczeń matematycznych, takich jak Python z NumPy, które oferują wbudowane, wysoko zoptymalizowane operacje do obsługi liczb zespolonych. Niemniej jednak, zrozumienie i implementacja podstawowych operacji w Google Apps Script jest użytecznym ćwiczeniem dla tych, którzy chcą poszerzyć swoje umiejętności programistyczne i zastosować je w szerokim zakresie kontekstów.
