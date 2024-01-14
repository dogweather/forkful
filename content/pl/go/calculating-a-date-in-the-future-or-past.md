---
title:    "Go: Obliczanie daty w przyszłości lub przeszłości"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Dlaczego obliczanie daty w przeszłości lub przyszłości jest ważne w programowaniu Go? 

Obliczanie daty w przeszłości lub przyszłości jest ważnym elementem programowania w dowolnym języku, w tym również w Go. Może to być niezbędne w celu wyświetlenia informacji o określonym dniu, czy też do zaplanowania działań na przyszłość. Dzięki możliwościom języka Go, obliczanie dat jest proste i wygodne, co jeszcze bardziej zachęca do jego wykorzystania.

## Jak to zrobić?

Aby obliczyć datę w przeszłości lub przyszłości w języku Go, wystarczy wykorzystać strukturę czasu (time) oraz funkcje z pakietu time. Przykładem może być obliczenie daty 7 dni temu i 7 dni w przód:

```Go
// importowanie pakietu time
import "time"

// obliczenie daty 7 dni temu
pastDate := time.Now().AddDate(0, 0, -7)

// obliczenie daty 7 dni w przód
futureDate := time.Now().AddDate(0, 0, 7)

// wyświetlenie wyników
fmt.Println("Data 7 dni temu: ", pastDate.Format("2006-01-02"))
fmt.Println("Data 7 dni w przód: ", futureDate.Format("2006-01-02"))
```

W powyższym kodzie, wykorzystano funkcję `AddDate()` do dodawania lub odejmowania określonej liczby lat, miesięcy i dni do bieżącej daty. Wyświetlanie wyników odbyło się z wykorzystaniem funkcji `Format()`, która pozwala na wyświetlenie daty w wybranym formacie, w tym przypadku `2006-01-02`.

Możliwości obliczania daty w przeszłości lub przyszłości w języku Go są naprawdę szerokie i warto wykorzystać je do ułatwienia swojej pracy.

## Głębsze zagłębienie

W języku Go istnieje również możliwość obliczania daty na podstawie podawanej liczby sekund. Do tego celu służy funkcja `Unix()` z pakietu time. Przykładem może być obliczenie daty 1000 sekund od teraz:

```Go
// importowanie pakietu time
import "time"

// obliczenie daty 1000 sekund od teraz
futureDate := time.Now().Add(time.Second * 1000)

// wyświetlenie wyniku
fmt.Println("Data 1000 sekund od teraz: ", futureDate.Format("2006-01-02 15:04:05"))
```

Wynik powyższego kodu będzie zawierać nie tylko datę, ale również godzinę, ponieważ wykorzystano większy zakres danych w funkcji `Format()`.

Zapoznając się z dokumentacją języka Go, można znaleźć także wiele innych funkcji do obliczania daty w przeszłości lub przyszłości, np. wykorzystanie strefy czasowej, uwzględnianie dni roboczych, itp. Warto zapoznać się z nimi, aby precyzyjnie dostosować obliczenia do swoich potrzeb.

# Zobacz również

- Dokumentacja pakietu time w języku Go: https://golang.org/pkg/time/
- Poradnik obliczania dat w języku Go: https://www.calhoun.io/working-with-dates-and-times-in-go/
- Przewodnik po strefach czasowych w języku Go: https://blog.golang.org/go-slices-usage-and-internals