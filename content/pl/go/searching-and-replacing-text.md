---
title:    "Go: Wyszukiwanie i zamiana tekstu"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasami w naszym kodzie musimy dokonać zmian w tekście, np. zmiana nazwy funkcji lub zmiana wszystkich wystąpień danej zmiennej. W takich sytuacjach, przydatne jest narzędzie do wyszukiwania i zamiany tekstu. W języku Go mamy kilka sposobów, aby to zrobić, a w tym artykule dowiesz się, jak to zrobić.

## Jak to zrobić

Najprostszym sposobem jest użycie funkcji strings.Replace ().

```Go
strings.Replace("Hello world!", "world", "Go", -1)
```

W powyższym przykładzie dokonujemy zamiany tekstu "world" na "Go" w zdaniu "Hello world!". Opcja -1 oznacza, że chcemy dokonać tej zmiany we wszystkich wystąpieniach, jeśli byśmy chcieli dokonać tylko jednej zmiany, wystarczyłoby użyć opcji 1.

Możemy także użyć biblioteki regexp do bardziej zaawansowanej wyszukiwania i zamiany tekstu. Na przykład, jeśli chcielibyśmy zmienić wszystkie liczby w tekście na "x", możemy to zrobić za pomocą wyrażeń regularnych:

```Go
str := "1-2-3-4-5"
re := regexp.MustCompile("[0-9]")
newStr := re.ReplaceAllString(str, "x")
fmt.Println(newStr)
```

Powiązany z biblioteką regexp jest również narzędzie gofix, które pozwala dokonać zmian w plikach zgodnie z danym wyrażeniem regularnym.

## Deep Dive

Język Go posiada także wiele innych funkcji i narzędzi do wyszukiwania i zamiany tekstu, takich jak strings.Index(), strings.Trim(), czy strings.Fields(). Jest to szczególnie przydatne w przypadku, gdy pracujemy z dużymi fragmentami tekstu lub plikami.

Ponadto, Go oferuje również możliwość równoległego przetwarzania tekstów za pomocą goroutines, co może znacząco przyspieszyć operacje wyszukiwania i zamiany.

## Zobacz także

- Dokumentacja funkcji strings.Replace w języku Go: https://pkg.go.dev/strings#Replace
- Dokumentacja biblioteki regexp w języku Go: https://pkg.go.dev/regexp
- Informacje o narzędziu gofix: https://golang.org/cmd/gofix/