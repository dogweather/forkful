---
title:                "Пошук та заміна тексту"
html_title:           "C++: Пошук та заміна тексту"
simple_title:         "Пошук та заміна тексту"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Введення в Пошук та Заміну в Go 

## Що і Навіщо?
Пошук та заміна тексту - це процес заміни одного фрагмента тексту іншим. Програмісти роблять це для автоматизації та оптимізації рутинних операцій над текстом.

## Як це робити:
```Go
package main
import (
	"fmt"
	"strings"
)

func main() {
	str := "Привіт, світ"
	newStr := strings.Replace(str, "світ", "Go", -1)

	fmt.Println(newStr)
}
```
Цей код замінює слово "світ" на слово "Go". Вивід: "Привіт, Go".

## Поглиблення
- **Історичний контекст**: Пошук та заміна тексту були частиною мов програмування ще з перших днів інформатики. Go, у свою чергу, надає простий та лаконічний інструмент для цього.
- **Альтернативи**: У замісь `strings.Replace`, ви також можете скористатися `regexp.ReplaceAllString`, якщо вам потрібно робити заміни, використовуючи регулярні вирази.
- **Деталі реалізації**: Go використовує ефективний алгоритм Кнута-Морріса-Пратта для пошуку підрядків, що робить `strings.Replace` вельми швидким.

## Дивіться ще 
1. [Dokumentatsiya po biblioteke strings](https://golang.org/pkg/strings/)
2. [Dokumentatsiya po regularnym vyrazheniyam v Go](https://golang.org/pkg/regexp/)