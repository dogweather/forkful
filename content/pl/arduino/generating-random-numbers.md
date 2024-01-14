---
title:                "Arduino: Generowanie losowych liczb"
programming_language: "Arduino"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Dlaczego generowanie losowych liczb jest ważne?

Generowanie losowych liczb jest bardzo przydatne w programowaniu Arduino. Pozwala ono na stworzenie różnorodnych przypadkowych scenariuszy, które mogą być wykorzystane w różnych projektach. Na przykład, w symulacji gry lub losowym wyborze elementów.

# Jak to zrobić?

Generowanie losowych liczb w Arduino jest bardzo proste, wystarczy wykorzystać funkcję "random()". Można to zrobić w ten sposób:

```Arduino 

// Stworzenie zmiennej, która będzie przechowywać wylosowaną liczbę
int randomNumber;

// Wywoływanie funkcji random() z ustaleniem przedziału od 1 do 10
// Wylosowana liczba zostanie przypisana do zmiennej randomNumber
randomNumber = random(1, 10);

// Wyświetlenie wylosowanej liczby w monitorze szeregowym
Serial.println(randomNumber);
```

W powyższym przykładzie, liczba wylosowana może przyjmować wartości od 1 do 10, ale można również zmienić zakres według własnych potrzeb.

# Deep Dive

Funkcja random() w Arduino jest w stanie wylosować liczby z bardzo dużego zakresu, aż do 4,2 miliarda. Jest to możliwe dzięki wykorzystaniu wbudowanej w mikrokontroler licznika. Licznik ten jest związany z częstotliwością pracy mikrokontrolera, dzięki czemu uzyskuje się losowe liczby.

Warto również wspomnieć, że funkcja random() nie jest w pełni losowa, a jedynie pseudolosowa. Oznacza to, że kolejne wywołania funkcji z tym samym seedem (wartością początkową) zwrócą ten sam ciąg liczb. Seed można zmienić, korzystając z funkcji randomSeed().

# Zobacz również

* Dokumentacja funkcji  [random()](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/)
* Poradnik na temat generowania losowych liczb w Arduino [Link](https://randomnerdtutorials.com/arduino-random-numbers-plotting/)
* Przykłady projektów wykorzystujących generowanie losowych liczb [Link](https://www.instructables.com/circuits/arduino/projects/random/)