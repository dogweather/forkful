---
title:                "Начало нового проекта"
date:                  2024-01-29T00:02:58.954305-07:00
model:                 gpt-4-0125-preview
simple_title:         "Начало нового проекта"
programming_language: "Go"
category:             "Go"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/go/starting-a-new-project.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Запуск нового проекта означает создание основы для вашего приложения на Go. Программисты делают это для организации кода, управления зависимостями и подготовки основы для дальнейшей разработки.

## Как это сделать:
Во-первых, установите Go, если вы еще этого не сделали, с [golang.org](https://golang.org/dl/). Затем настройте новый проект:

1. Откройте терминал.
2. Создайте новую директорию.

   ```bash
   mkdir myproject
   cd myproject
   ```

3. Инициализируйте модуль:

   ```bash
   go mod init github.com/yourusername/myproject
   ```

4. Напишите простой файл `main.go`:

   ```Go
   package main

   import "fmt"

   func main() {
       fmt.Println("Привет, новый мир Go!")
   }
   ```

5. Запустите программу:

   ```bash
   go run main.go
   ```

Пример вывода должен быть:

```
Привет, новый мир Go!
```

## Глубокое погружение
Запуск нового проекта на Go эволюционировал. Ранние проекты на Go не имели официальной системы управления пакетами. Это привело к появлению модели рабочего пространства "GOPATH", которая могла стать запутанной при работе над крупными проектами. На данный момент, с введением `go mod` в Go 1.11, процесс стал более упорядоченным и управляемым: зависимости обрабатываются для каждого проекта отдельно, а не глобально.

Альтернативы `go mod` постепенно исчезают, но ранее включали в себя инструменты сообщества, такие как `dep` и `glide`. Сегодня `go mod` рекомендуется в качестве инструмента благодаря его поддержке первой стороной и интеграции с инструментарием Go.

Когда вы запускаете `go mod init`, Go создает новый файл `go.mod`. Этот файл отслеживает зависимости вашего проекта. Он автоматически перечисляет версию Go и любые внешние пакеты, которые вы добавляете позднее. С этой настройкой зависимости вашего кода являются явными и воспроизводимыми, что помогает избежать синдрома "работает у меня на машине".

## См. также
- [Начало работы с Go](https://golang.org/doc/install)
- [Как писать код на Go](https://golang.org/doc/code.html)
- [Документация `go mod`](https://golang.org/ref/mod)