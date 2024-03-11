---
date: 2024-01-26 03:43:21.079956-07:00
description: "Zaokr\u0105glanie liczb to redukcja liczby dziesi\u0119tnej do najbli\u017C\
  szej warto\u015Bci ca\u0142kowitej lub do okre\u015Blonej liczby miejsc po przecinku.\
  \ Programi\u015Bci zaokr\u0105glaj\u0105\u2026"
lastmod: '2024-03-11T00:14:08.863816-06:00'
model: gpt-4-0125-preview
summary: "Zaokr\u0105glanie liczb to redukcja liczby dziesi\u0119tnej do najbli\u017C\
  szej warto\u015Bci ca\u0142kowitej lub do okre\u015Blonej liczby miejsc po przecinku.\
  \ Programi\u015Bci zaokr\u0105glaj\u0105\u2026"
title: "Zaokr\u0105glanie liczb"
---

{{< edit_this_page >}}

## Co i dlaczego?
Zaokrąglanie liczb to redukcja liczby dziesiętnej do najbliższej wartości całkowitej lub do określonej liczby miejsc po przecinku. Programiści zaokrąglają liczby, aby były łatwiejsze do odczytania i obsługi, szczególnie gdy dokładność poza pewnym punktem jest niepotrzebna lub mogłaby prowadzić do błędów.

## Jak to zrobić:
W Arduino możesz zaokrąglać liczby używając wbudowanych funkcji. Kluczowe są `round`, `ceil` i `floor`. Oto krótka demonstracja:

```arduino
void setup() {
  Serial.begin(9600);
  
  float myNumber = 123.4567;

  // Zaokrąglij do najbliższej liczby całkowitej
  Serial.println(round(myNumber)); // Wyświetla: 123

  // Zawsze zaokrągla w górę
  Serial.println(ceil(myNumber));  // Wyświetla: 124

  // Zawsze zaokrągla w dół
  Serial.println(floor(myNumber)); // Wyświetla: 123
}

void loop() {
  // Nic do pętlowania.
}
```

## Dogłębna analiza:
Algorytmy zaokrąglania mają długą historię; były stosowane na długo przed cyfrowymi komputerami. W obliczeniach analogowych zaokrąglenie było procesem fizycznym. W obliczeniach cyfrowych jest matematyczne.

Zaokrąglenie jest potrzebne, gdy konwertujemy z typu o większej precyzji (jak `float` lub `double`) na typ o mniejszej precyzji (jak `int`). Ale sposób zaokrąglania może się różnić:

1. `round()`: Standardowe zaokrąglanie. Jeśli ułamek wynosi 0,5 lub więcej, zaokrągla w górę; w przeciwnym razie w dół.
2. `ceil()`: Krótko mówiąc, "sufit", zawsze zaokrągla w górę do najbliższej liczby całkowitej, nawet jeśli jest bliżej niższej liczby.
3. `floor()`: Przeciwieństwo sufitu; zawsze zaokrągla w dół.

Wybór między tymi funkcjami zależy od tego, do czego potrzebna jest zaokrąglona wartość. Pomiary mogą wymagać standardowego zaokrąglenia, pieniądze często korzystają z `floor`, podczas gdy systemy inwentaryzacyjne mogą używać `ceil`, aby zapewnić, że wszystko jest uwzględnione.

Implementacja tych funkcji przez Arduino jest prosta; nie obsługują dodatkowych przypadków, takich jak zaokrąglanie do określonych miejsc dziesiętnych. W takim przypadku w grę wchodzi stworzenie własnej funkcji lub głębsza matematyka – myśl o mnożeniu, aby przesunąć miejsce dziesiętne, zaokrąglić, a następnie podzielić z powrotem.

Błędy zaokrąglenia mogą się kumulować, znacząco wpływając na długie obliczenia lub procesy iteracyjne. Programiści muszą być ostrożni, przeprowadzając liczne operacje na zaokrąglonych wartościach.

## Zobacz również:
2. Dogłębny przegląd pułapek i strategii zaokrąglania: [Przewodnik po liczbach zmiennoprzecinkowych](https://floating-point-gui.de/)
3. Dla zaawansowanych technik, w tym własnych funkcji zaokrąglania i obsługi błędów zaokrąglenia, warto sprawdzić źródła akademickie lub szczegółowe przewodniki programistyczne.
