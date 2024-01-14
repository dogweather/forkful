---
title:    "Javascript: Drumienie wyników debugowania"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego

Podczas programowania często spotykamy się z błędami i problemami, które utrudniają nam działanie naszych programów. Czasami jednak trudno jest zlokalizować przyczynę tych problemów. Właśnie dlatego warto zacząć stosować w swoim kodzie wypisywanie informacji diagnostycznych, zwanych także "debug output". Pozwala to nam na śledzenie przebiegu programu, analizowanie zawartości zmiennych i łatwiejsze znalezienie źródła problemów. W tym artykule dowiesz się, dlaczego warto stosować wypisywanie debug output oraz jak to zrobić w języku Javascript, na przykładach kodu.

## Jak to zrobić

Aby wypisywać debug output w języku Javascript, wystarczy skorzystać z metody ```console.log()```, która pozwala nam wypisać podaną jako argument zawartość. Możemy w ten sposób wypisać tekst, wartości zmiennych, a nawet obiekty i tablice. Przykład:

```Javascript
const name = "Adam";
const age = 24;

console.log("Witaj, nazywam się " + name + " i mam " + age + " lata.");

// Wynik:
// Witaj, nazywam się Adam i mam 24 lata.
```

Możemy także wypisać więcej niż jeden argument, oddzielać je przecinkami lub używać wyrażeń szablonowych (template literals). Przykład z użyciem wyrażeń szablonowych:

```Javascript
const fruits = ["jabłko", "gruszka", "ananas"];
const count = 3;

console.log(`Liczba owoców: ${count}. Owocami są: ${fruits.join(", ")}.`);

// Wynik:
// Liczba owoców: 3. Owocami są: jabłko, gruszka, ananas.
```

Możemy także sprawdzić zawartość obiektów lub tablic, wyświetlając całą ich strukturę. Przykład:

```Javascript
const person = {
  name: "Kasia",
  age: 33,
  hobbies: ["programowanie", "joga", "podróże"]
};

console.log(person);

// Wynik:
// {
//   name: "Kasia",
//   age: 33,
//   hobbies: ["programowanie", "joga", "podróże"]
// }
```

Dzięki wypisywaniu debug output w wybranych miejscach w naszym kodzie, możemy łatwiej śledzić jego przebieg, analizować wartości zmiennych w poszczególnych punktach i łatwiej znajdować błędy.

## Deep Dive

Wypisywanie debug output jest szczególnie przydatne podczas pisania bardziej skomplikowanych aplikacji lub rozwiązywania problemów w istniejącym kodzie. Dzięki temu możemy szybciej zlokalizować błędy i znacznie skrócić czas debugowania. Możemy także wypisywać informacje diagnostyczne w zależności od tego, w jakich warunkach wykonuje się nasz kod, co pozwala nam na lepsze zrozumienie jego działania.

Pamiętaj jednak, żeby nie zostawiać wypisywania debug output w swoim kodzie na stałe. Służy ono jedynie jako narzędzie do pomocy w debugowaniu, a nie powinno być częścią finalnego produktu. Warto więc ustawić sobie flagę (np. o wartości "true" lub "false"), dzięki której w łatwy sposób możemy włączać i wyłączać wypisywanie debug output w naszym kodzie.

## Zobacz też

- [Dlaczego warto stosować debug output (ang. Why You Should Use Debug Outputs)](https://codeburst.io/why-you-should-use-debug-outputs-ab85862a1b5e)
- [Funkcja console.log() w języku Javascript](https://developer.mozilla.org/pl/docs/Web/API