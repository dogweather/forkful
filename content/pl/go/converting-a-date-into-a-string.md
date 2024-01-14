---
title:    "Go: Konwertowanie daty do ciągu znaków"
keywords: ["Go"]
---

{{< edit_this_page >}}

# Dlaczego

Konwersja daty na ciąg znaków jest częstą operacją w programowaniu, szczególnie w przypadku tworzenia aplikacji internetowych. W ten sposób można wyświetlić datę w czytelny sposób dla użytkowników lub przetwarzać ją w celu wykorzystania w innych operacjach. W tym artykule pokażemy, jak w łatwy sposób przeprowadzić konwersję daty na ciąg znaków w języku Go.

# Jak to zrobić

Aby przekonwertować datę na ciąg znaków w języku Go, należy użyć funkcji `Format()` z pakietu `time`. Najpierw musimy utworzyć zmienną typu `time.Time`, która będzie przechowywać datę, którą chcemy przekonwertować. Następnie za pomocą funkcji `Format()` możemy ustalić żądany format daty. Na przykład:

```
Go czas := czas.NowaData(2021, czas.Kwiecień, 15)
data := czas.Format("02.01.2006")
fmt.Println(data) //Wyjście: 15.04.2021
```

W powyższym przykładzie użyliśmy formatu "02.01.2006", który oznacza, że chcemy wyświetlić datę w formacie dzień.miesiąc.rok (w amerykańskim systemie zapisu daty). Istnieje wiele różnych symboli, które można użyć w funkcji `Format()` aby ustalić pożądany format. Przykładowe symbole to:

- `02`: dwa miejsca na dzień (wiodąca 0 jest dodawana dla jednocyfrowych dni)
- `01`: dwa miejsca na miesiąc (wiodąca 0 jest dodawana dla jednocyfrowych miesięcy)
- `2006`: cztery miejsca na rok (wiodące zera są dodawane dla dwu- i jednocyfrowych lat)
- `15`: dwie cyfry na godzinę (wiodąca 0 jest dodawana dla jednocyfrowych godzin)
- `04`: dwie cyfry na minutę (wiodąca 0 jest dodawana dla jednocyfrowych minut)
- `05`: dwie cyfry na sekundę (wiodąca 0 jest dodawana dla jednocyfrowych sekund)

Aby poznać więcej możliwych symboli, można zajrzeć do dokumentacji pakietu `time` w języku Go.

# Pogłębione zagadnienia

Powyższy przykład jest tylko jednym ze sposobów na przekonwertowanie daty na ciąg znaków w języku Go. Istnieją również inne funkcje, takie jak `Unix()` czy `LongSuit()` które pozwalają na ustawienie własnych formatów i wyświetlenie dodatkowych informacji, takich jak strefa czasowa czy dzień tygodnia.

Warto również pamiętać o istnieniu różnych metod parsowania daty z ciągu znaków w języku Go. Jest to przydatne, gdy przetwarzamy dane, które otrzymujemy z zewnętrznych źródeł, które mogą mieć inny format daty niż oczekujemy.

# Zobacz również

- Dokumentacja pakietu `time` w języku Go: https://golang.org/pkg/time/
- Przewodnik po formatowaniu daty w języku Go: https://gobyexample.com/time-formatting-parsing
- Poradnik na temat konwersji daty i czasu w języku Go: https://www.calhoun.io/working-with-dates-and-times-in-go/