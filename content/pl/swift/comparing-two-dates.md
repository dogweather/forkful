---
title:    "Swift: Porównywanie dwóch dat"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Często w programowaniu musimy porównywać daty w celu sprawdzenia, która jest wcześniejsza lub późniejsza. Jest to ważna umiejętność w wielu aplikacjach, zwłaszcza w tych, które mają funkcje związane z czasem. W tym artykule dowiesz się, jak porównywać daty w języku Swift.

## Jak to zrobić

Język Swift oferuje nam różne sposoby na porównywanie dat. Możemy porównać daty według ich kolejności, czyli która jest wcześniejsza, a która późniejsza. Możemy również sprawdzić, czy dwie daty są takie same. Oto kilka kodów przykładowych, które pozwolą Ci zrozumieć, jak to działa:

### Porównywanie dat według kolejności

```Swift
let date1 = Date()
let date2 = Date(timeIntervalSinceNow: 86400) // 1 dzień po date1

if date1 < date2 { // używając < porównujemy, która data jest wcześniejsza
  print("Date1 jest wcześniejsza od date2.")
} else {
  print("Date2 jest wcześniejsza od date1.")
}
```
Output: Date1 jest wcześniejsza od date2.

### Sprawdzanie, czy daty są takie same

```Swift
let date1 = Date()
let date2 = Date(timeIntervalSinceNow: 86400) // 1 dzień po date1

if date1 == date2 { // używając == sprawdzamy, czy daty są takie same
  print("Daty są takie same.")
} else {
  print("Daty są różne.")
}
```
Output: Daty są różne.

## Zagłębienie

Porównywanie dat może być bardziej skomplikowane w zależności od tego, w jakiej formie są przechowywane. W języku Swift mamy do dyspozycji typy Date i Calendar, które mogą pomóc nam w manipulacji i porównywaniu dat. Możemy również używać funkcji, takich jak compare lub isEqual, aby porównać daty w bardziej zaawansowany sposób.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej o porównywaniu dat w języku Swift, możesz przeczytać te artykuły:

- ["Porównywanie dat w języku Swift" od Hacking with Swift](https://www.hackingwithswift.com/example-code/nsdate/how-to-compare-dates)
- ["Porównywanie dat w Swift" od Ray Wenderlich](https://www.raywenderlich.com/21-how-to-compare-dates-in-swift)
- ["Porównywanie dat w języku Swift: Co trzeba wiedzieć" od LearnAppMaking](https://learnappmaking.com/compare-dates-swift/)

Teraz powinieneś być w stanie porównywać daty w swoich projektach w języku Swift. Pamiętaj, że odporne na błędy i trwałe aplikacje to te, które poprawnie porównują daty. Dzięki temu informacje dotyczące czasu w Twojej aplikacji będą zawsze dokładne i aktualne. Powodzenia w kodowaniu!