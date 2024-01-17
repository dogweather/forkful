---
title:                "Pobieranie aktualnej daty"
html_title:           "Go: Pobieranie aktualnej daty"
simple_title:         "Pobieranie aktualnej daty"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Co to jest & Dlaczego?

Aktualna data jest informacją o bieżącej dacie, czasie i strefie czasowej. Programiści wykorzystują ją do śledzenia i wyświetlania aktualnego czasu w aplikacjach, automatycznego datowania informacji oraz do porównywania różnych danych.

## Jak to zrobić:

```
Go get current date and time:

package main

import (
	"fmt"
	"time"
)

func main() {
	now := time.Now()
	fmt.Println("Current Date and Time:", now)
}

Output: Current Date and Time: 2021-05-08 12:00:00.000000000 +0000 UTC m=+0.000000001 
```

## Głębsze zanurzenie:

Aktualna data jest dostępna w języku Go dzięki wbudowanej bibliotece "time". Wcześniej programiści musieli polegać na systemowym zegarze systemu operacyjnego, aby uzyskać bieżącą datę i czas. Jednak dzięki bibliotece "time", mogą teraz łatwo i precyzyjnie uzyskać bieżący czas bez uzależnienia się od systemu operacyjnego. Alternatywnym sposobem uzyskiwania aktualnej daty jest wykorzystanie biblioteki "date", jednak jest ona mniej precyzyjna i wymaga większej liczby linii kodu.

## Zobacz też:

https://pkg.go.dev/time - Oficjalna dokumentacja biblioteki "time" w języku Go.