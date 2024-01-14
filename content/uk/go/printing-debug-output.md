---
title:                "Go: Виведення відлагоджувального виводу"
simple_title:         "Виведення відлагоджувального виводу"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## Чому

Навіщо вам знадобиться вивід дебагу в програмуванні? Це просте: вивід дебагу допомагає вам зрозуміти, як ваша програма працює та виявити помилки, що можуть бути приховані в коді.

## Як

Щоб вивести дебаг, використовуйте вбудовану функцію "print". Наприклад, вам потрібно вивести значення змінної "x". У Go це виглядатиме так: 

```Go
print(x)
```

Можна також вивести кілька значень за допомогою коми:

```Go
print(x, y, z)
```

Також існує можливість використовувати функцію "fmt.Printf", яка дозволяє використовувати форматування виводу. Наприклад, ви можете вивести число з плаваючою точкою з двома знаками після крапки:

```Go
fmt.Printf("Число: %.2f", x)
```

## Глибокий занурення

Вивід дебагу може бути корисним не лише для виведення значень змінних, але і для виведення повідомлень про те, як програма працює. Наприклад, ви можете додати таке повідомлення перед виведенням значення змінної "x":

```Go
print("Значення x: ", x)
```

Це допоможе вам легше зрозуміти, де саме в коді відбуваються зміни значень.

## Дивіться також

- [Використання функції "print" в Go](https://golang.org/pkg/fmt/#Print)
- [Використання функції "fmt.Printf" в Go](https://golang.org/pkg/fmt/#Printf)
- [Створення повідомлень для виводу дебагу в Go](https://blog.golang.org/strings)

## Дивіться також

- [Printing debug output in Go using the "print" function](https://golang.org/pkg/fmt/#Print)
- [Using the "fmt.Printf" function in Go](https://golang.org/pkg/fmt/#Printf)
- [Creating debug messages in Go](https://blog.golang.org/strings)