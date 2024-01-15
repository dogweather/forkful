---
title:                "Generowanie losowych liczb"
html_title:           "Go: Generowanie losowych liczb"
simple_title:         "Generowanie losowych liczb"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Dlaczego?

Generowanie liczb losowych jest często wykorzystywane w programowaniu w celu symulacji różnych scenariuszy lub do tworzenia unikatowych identyfikatorów. Jest to również ważny element wielu gier komputerowych czy aplikacji mobilnych. W języku Go istnieje dedykowany pakiet "math/rand", który umożliwia generowanie liczb losowych.

# Jak to zrobić?

Wygenerowanie losowej liczby w języku Go jest bardzo proste i sprowadza się do kilku kroków. Pierwszym krokiem jest importowanie pakietu "math/rand". Następnie należy użyć funkcji "Seed" w celu ustawienia ziarna dla generatora liczb pseudolosowych. W końcu, używając funkcji "Intn", możemy wygenerować liczbę losową w określonej przez nas przedziale. Oto przykładowy kod:

```Go
import "math/rand"

func main() {
	rand.Seed(time.Now().UnixNano())
	randomNumber := rand.Intn(100)
	fmt.Println(randomNumber)
}
```

W powyższym przykładzie, korzystając z funkcji "Intn(100)", wygenerujemy liczbę losową w zakresie od 0 do 99. Możemy także wykorzystać funkcję "Intn(100) + 1", aby wygenerować liczbę losową z zakresu od 1 do 100.

# Deep Dive

Generator liczb pseudolosowych w pakiecie "math/rand" opiera się na algorytmie "linear congruential generator" (LCG). Polega on na generowaniu kolejnych liczb pseudolosowych na podstawie poprzedniej liczby według wzoru:

```Go
Xn+1 = (a * Xn + c) mod m
```

gdzie:
- Xn - kolejna liczba pseudolosowa
- a, c, m - stałe określające zachowanie generatora liczb pseudolosowych

Znak "mod" oznacza resztę z dzielenia. W przypadku pakietu "math/rand" stałe są ustalone na:
- a = 6364136223846793005
- c = 1442695040888963407
- m = 1 << 63

Ponadto, w celu uzyskania większej różnorodności liczb losowych, generator ten używa zmiennej "src" przechowującej informacje o aktualnym stanie. Każde wywołanie funkcji "Intn" powoduje modyfikację tej zmiennej i generację nowej liczby pseudolosowej.

# Zobacz także

Jeśli chcesz dowiedzieć się więcej o pakiecie "math/rand" i sposobie generowania liczb pseudolosowych w języku Go, polecamy zapoznać się z dokumentacją oficjalną: 
- https://golang.org/pkg/math/rand/
- https://blog.golang.org/go-ranges
- https://gobyexample.com/random-numbers