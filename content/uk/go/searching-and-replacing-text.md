---
title:                "Go: Пошук та заміна тексту"
simple_title:         "Пошук та заміна тексту"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Чому

У програмуванні, часто звертаються до змінення тексту для виправлення помилок, оновлення даних або забезпечення однаковості формату. Використання пошуку та заміни тексту допомагає ефективно та швидко здійснювати подібні завдання.

# Як

Щоб здійснити пошук та заміну тексту в Go, використовуємо функцію `strings.Replace()`.

```Go
package main
import "fmt"
import "strings"

func main() {
    oldString := "Привіт, світ!"
    newString := strings.Replace(oldString, "Привіт", "Вітаю", 1)
    fmt.Println(newString)
}
```

Вивід: `Вітаю, світ!`

# Глибше

Функція `strings.Replace()` має наступний синтаксис: `Replace(s, old, new string, n int) string`, де `s` - вихідний рядок, `old` - підрядок, який треба замінити, `new` - підрядок, на який треба замінити, `n` - кількість замін, за замовчуванням замінюється всі входження. 

Також можна використовувати функцію `strings.ReplaceAll()`, яка замінює всі входження підрядка без обмеження кількості замін.

```Go
package main
import "fmt"
import "strings"

func main() {
    oldString := "Привіт, світ!"
    newString := strings.ReplaceAll(oldString, "і", "и")
    fmt.Println(newString)
}
```

Вивід: `Привит, свит!`

# Дивіться також

- [Повна документація про пакет strings](https://golang.org/pkg/strings/)
- [Приклади роботи з текстом в Go](https://gobyexample.com/strings)