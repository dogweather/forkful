---
title:                "Wyszukiwanie i zamiana tekstu"
html_title:           "Go: Wyszukiwanie i zamiana tekstu"
simple_title:         "Wyszukiwanie i zamiana tekstu"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Zamiana i wyszukiwanie tekstu to ważne czynności w programowaniu. Pozwalają one na szybkie i łatwe zmiany w kodzie oraz odnajdywanie określonych fragmentów, co przyspiesza i ułatwia pracę programistów.

## Jak to zrobić:
Aby dokonać zamiany oraz wyszukać tekst w kodzie za pomocą Go, można skorzystać z funkcji "Replace" lub wykorzystać wyrażenia regularne. Przykłady zastosowania są przedstawione poniżej:

```Go
// Przykład użycia funkcji Replace
word := "hello"
newWord := strings.Replace(word, "e", "s", 1) // output: "hsllo"
```

```Go
// Przykład użycia wyrażeń regularnych w celu znalezienia określonego wzorca
text := "Znajdź mnie, jeśli jestem liczbą"
pattern := `\d+` // wzorzec pasujący do dowolnej liczby
found := regexp.MustCompile(pattern).FindString(text) // output: "znajdź mnie 123"
```

## Głębsze zagłębienie się w temat:
Zamiana i wyszukiwanie tekstu jest powszechnie stosowanym narzędziem zarówno przez programistów, jak i użytkowników. W przypadku Go, możliwość wykorzystania wyrażeń regularnych daje duże możliwości w celu dostosowania i dopasowania funkcji do swoich potrzeb. Alternatywnie, można również skorzystać z bibliotek takich jak "strings" lub "regexp", które oferują dodatkowe funkcjonalności związane z wyszukiwaniem i zamianą tekstu w kodzie.

## Zobacz też:
- Dokumentacja Go dotycząca funkcji Replace: https://golang.org/pkg/strings/#Replace
- Przykładowe wyrażenia regularne w Go: https://yourbasic.org/golang/regexp-cheat-sheet/
- Dokumentacja Go dotycząca wyrażeń regularnych: https://golang.org/pkg/regexp/