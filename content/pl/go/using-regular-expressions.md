---
title:    "Go: Korzystanie z wyrażeń regularnych"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Dlaczego

Wykorzystywanie wyrażeń regularnych w programowaniu jest niezwykle przydatne i pozwala na szybkie i precyzyjne przetwarzanie tekstu. Jest to szczególnie ważne w przypadku analizy danych czy weryfikacji danych wejściowych. Dzięki wyrażeniom regularnym można wykrywać wzorce w tekście, co znacznie ułatwia proces programowania.

## Jak

Aby skorzystać z wyrażeń regularnych w języku Go, należy zastosować pakiet "regexp". Najpierw tworzymy wyrażenie regularne przy użyciu funkcji "Compile", a następnie wykorzystujemy go w celu przetworzenia tekstu za pomocą funkcji "FindString" lub "FindAllString". Poniżej znajdują się przykładowe kody oraz odpowiadające im wyjścia.

```Go
// Przykład 1: Znajdowanie wszystkich cyfr w tekście
re := regexp.Compile("[0-9]+")
text := "Mały piesek ma 2 lata."
result := re.FindAllString(text, -1)
fmt.Println(result)
// Wynik: [2]

// Przykład 2: Weryfikacja adresu e-mail
re := regexp.Compile("[a-z0-9]+@[a-z]+\\.[a-z]+")
email := "janek123@poczta.com"
isValid := re.MatchString(email)
if isValid {
  fmt.Println("Adres e-mail jest poprawny")
} else {
  fmt.Println("Niepoprawny adres e-mail")
}
// Wynik: Adres e-mail jest poprawny
```

## Głębokie zanurzenie

Wyrażenia regularne w języku Go mają wiele zaawansowanych funkcji, takich jak grupy, alternatywy czy kwantyfikatory, które pozwalają na jeszcze precyzyjniejsze przetwarzanie tekstu. Warto zapoznać się z dokumentacją języka Go, aby poznać wszystkie możliwości wyrażeń regularnych oraz dowiedzieć się, jak wykorzystać je w różnych sytuacjach.

## Zobacz też

- [Dokumentacja języka Go w zakresie wyrażeń regularnych](https://golang.org/pkg/regexp/)
- [Wzorce i wyrażenia regularne w języku Go](https://blog.golang.org/regular-expressions)
- [Poradnik dla początkujących w zakresie wyrażeń regularnych w języku Go](https://www.calhoun.io/regular-expressions-in-go/)