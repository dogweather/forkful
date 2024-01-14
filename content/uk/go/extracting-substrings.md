---
title:    "Go: Вилучення підрядків"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## Чому

Велма часто потрібно витягувати частини тексту зі стрічок, наприклад, для пошуку підрядка або форматування. У цьому блозі ми розглянемо те, як виконувати цю задачу в Go.

## Як

```Go
// Завантажуємо пакет для роботи з рядками
import "strings"

func main() {
	//Стрічка, з якої ми будемо витягувати підрядок
	str := "Привіт, це блог про Go"
	// Витягуємо підрядок з шостого символу до кінця
	substr := strings.TrimLeft(str, 5)
	fmt.Println(substr)
}
```
Результат:
```
т, це блог про Go
```

## Глибоке погруження

Використання функції `strings.TrimLeft` дозволяє виконувати різноманітні операції з рядками, такі як отримання підрядка, видалення пробілів та інше. Більше інформації про цю та інші корисні функції можна знайти в [документації](https://golang.org/pkg/strings/).

## Дивись також

- [Робота з рядками в Go](https://golang.org/doc/effective_go.html#strings)
- [Приклади витягування підрядків в Go](https://gobyexample.com/substrings)
- [Програмування на Go: Повний посібник](https://golangbot.com/learn-golang-series/)