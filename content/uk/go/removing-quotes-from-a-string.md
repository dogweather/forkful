---
title:                "Видалення лапок зі строки"
date:                  2024-01-26T03:39:56.203415-07:00
model:                 gpt-4-0125-preview
simple_title:         "Видалення лапок зі строки"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Що та Чому?

Видалення лапок з рядка означає позбавлення від тих нав'язливих подвійних або одинарних лапкових символів, які обгортають ваш справжній текст. Ми робимо це для санітарної очистки даних, запобігання помилкам аналізу або підготовки тексту до подальшої обробки без зайвого мотлоху лапок.

## Як це зробити:

Ось простий спосіб позбутися лапок у Go:

```go
package main

import (
	"fmt"
	"strings"
)

func removeQuotes(s string) string {
	return strings.Trim(s, "'\"")
}

func main() {
	quotedString := "\"Привіт, Світ!\""
	fmt.Println("Оригінал:", quotedString)

	unquotedString := removeQuotes(quotedString)
	fmt.Println("Без лапок:", unquotedString)
}
```

Результат буде виглядати так, лапок немає:

```
Оригінал: "Привіт, Світ!"
Без лапок: Привіт, Світ!
```

## Поглиблений аналіз

У минулі часи, коли формати даних і обмін ними не були стандартизовані, лапки у рядках могли створювати хаос. Вони досі можуть, особливо в JSON або коли ви вставляєте рядки в бази даних. Пакет `strings` у Go містить функцію `Trim`, яка видаляє не тільки пробіли, але й будь-які символи, які вам не подобаються.

Чому не Regex? Ну, `Trim` швидший для простих завдань, але якщо ваші рядки грають у хованки з лапками в дивних місцях, regex може бути вашою важкою артилерією:

```go
import "regexp"

func removeQuotesWithRegex(s string) string {
	re := regexp.MustCompile(`^["']|["']$`)
	return re.ReplaceAllString(s, "")
}
```

Це як вибір між ножицями та ланцюговою пилкою; вибирайте інструмент, який підходить для роботи.

## Дивіться також

Для більшого розуміння пакету `strings` та його потужних інструментів:
- [Пакет strings](https://pkg.go.dev/strings)

Щоб оволодіти могутністю регулярних виразів у Go:
- [Пакет regexp](https://pkg.go.dev/regexp)

Хочете заглибитися у філософію обрізки рядків?
- [Метод Trim](https://blog.golang.org/strings)