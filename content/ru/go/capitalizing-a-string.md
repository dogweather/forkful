---
title:                "Преобразование строки в верхний регистр"
date:                  2024-01-28T23:55:33.975860-07:00
model:                 gpt-4-0125-preview
simple_title:         "Преобразование строки в верхний регистр"

category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/go/capitalizing-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Преобразование строки с заглавной буквы превращает первую букву данной строки в прописную букву. Программисты делают это для форматирования вывода, соблюдения грамматических правил или улучшения читаемости текста.

## Как это сделать:
В Go строки неизменяемы, поэтому вам нужно создать новую версию строки с заглавной буквы. Мы используем пакет `strings` и его функцию `Title` или напрямую манипулируем рунами строки:

```Go
package main

import (
	"fmt"
	"strings"
	"unicode"
)

func main() {
	// Пример 1: Использование strings.Title для преобразования каждого слова с заглавной буквы
	fmt.Println(strings.Title("hello world!")) // Выводит: Hello World!

	// Пример 2: Преобразование только первого символа
	input := "hello again!"
	if len(input) > 0 {
		fmt.Println(strings.ToUpper(string(input[0])) + input[1:]) // Выводит: Hello again!
	}

	// Пример 3: Более надёжное преобразование с заглавной буквы, учитывая многобайтовые символы
	capitalizeFirst := func(s string) string {
		for i, v := range s {
			return string(unicode.ToUpper(v)) + s[i+utf8.RuneLen(v):]
		}
		return ""
	}

	fmt.Println(capitalizeFirst("привет мир!")) // Выводит: Привет мир!
}
```

## Подробнее
Преобразование строки с заглавной буквы - не сложный процесс, но в его основе лежит много деталей. До существования функции `strings.Title`, для правильного преобразования приходилось манипулировать строками на уровне рун.

В старых языках программирования обработка не-ASCII символов при преобразовании была затруднена из-за отсутствия должной поддержки Unicode. Go облегчает это с встроенной поддержкой кодировки UTF-8 в стандартных пакетах `unicode` и `utf8`.

Когда вы вручную преобразуете строки в Go, помните об обработке многобайтовых символов. Вот почему мы используем цикл `range` в надёжном примере, который итерирует через руны, а не байты.

Существуют альтернативы встроенным методам Go, такие как использование сторонних библиотек для более сложных потребностей в манипуляции текстом. Однако для простого преобразования с заглавной буквы стандартной библиотеки Go обычно достаточно.

## Смотрите также
- Пакет строк Go: [https://golang.org/pkg/strings/](https://golang.org/pkg/strings/)
- Пакет unicode Go: [https://golang.org/pkg/unicode/](https://golang.org/pkg/unicode/)
- Пакет utf8 Go: [https://golang.org/pkg/unicode/utf8/](https://golang.org/pkg/unicode/utf8/)
- Крутая статья о строках и рунах в Go: [https://blog.golang.org/strings](https://blog.golang.org/strings)
