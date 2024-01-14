---
title:                "Go: Porównywanie dwóch dat"
programming_language: "Go"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Dlaczego porównywanie dwóch dat jest ważne w programowaniu w Go?

Porównywanie dwóch dat jest ważnym elementem w programowaniu w Go, ponieważ często musimy określić, która z dwóch dat jest wcześniejsza lub późniejsza. Może to być przydatne w przypadku sortowania danych lub wykonywania określonych działań w zależności od daty.

# Jak porównać dwie daty w Go?

Aby porównać dwie daty w Go, musimy użyć funkcji "Equal()" z pakietu "time". Najpierw musimy przekonwertować daty do typu "Time", a następnie porównać je za pomocą funkcji "Equal()". Poniższy kod przedstawia przykładowe porównanie dwóch dat i wyświetlenie wyniku:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	// Przykładowe daty do porównania
	date1 := time.Date(2021, time.August, 11, 0, 0, 0, 0, time.UTC)
	date2 := time.Date(2021, time.August, 10, 0, 0, 0, 0, time.UTC)

	// Porównanie dat
	if date1.Equal(date2) {
		fmt.Println("Daty są identyczne")
	} else if date1.After(date2) {
		fmt.Println("Pierwsza data jest późniejsza")
	} else if date1.Before(date2) {
		fmt.Println("Pierwsza data jest wcześniejsza")
	}
}
```

**Output:**

```
Pierwsza data jest późniejsza
```

# Głębsze spojrzenie na porównywanie dwóch dat w Go

Funkcja "Equal()" porównuje daty na podstawie dokładnej wartości, więc jeśli obie daty są różne o zaledwie kilka milisekund, zostanie zwrócona wartość "false". Dla wielu zastosowań może to być wystarczające, jednak jeśli potrzebujemy porównać daty z większą precyzją, możemy wykorzystać funkcję "Truncate()" do zaokrąglenia dat do jednostki czasu, na przykład do minut lub godzin.

Przykładowo, jeśli chcemy porównać daty pomijając różnicę w sekundach, możemy użyć funkcji "Truncate" dla obu dat, a następnie porównać je za pomocą funkcji "Equal()":

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	// Przykładowe daty do porównania
	date1 := time.Date(2021, time.August, 11, 14, 30, 21, 0, time.UTC)
	date2 := time.Date(2021, time.August, 11, 14, 30, 15, 0, time.UTC)

	// Porównanie dat
	if date1.Truncate(time.Minute).Equal(date2.Truncate(time.Minute)) {
		fmt.Println("Daty są identyczne z dokładnością do minuty")
	} else if date1.After(date2) {
		fmt.Println("Pierwsza data jest późniejsza")
	} else if date1.Before(date2) {
		fmt.Println("Pierwsza data jest wcześniejsza")
	}
}
```

**Output:**

```
Daty są identyczne z dokładnością do minuty
```

# Zobacz także

- Dokumentacja pakietu "time" w języku Go: https://golang.org/pkg/time/
- Przykładowy kod z porównaniem dwóch dat: https://play.golang.org/p/kXC_Gp3WW7g