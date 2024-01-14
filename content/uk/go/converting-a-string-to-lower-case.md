---
title:    "Go: Перетворення рядка на нижній регістр"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

Авторівнявати рядок до нижнього регістру у Go

## Why
Існує багато ситуацій, коли корисно перетворити рядок до нижнього регістру, наприклад, для порівняння тексту або для подальшої обробки даних.

## How To
Код для перетворення рядка до нижнього регістру виглядає наступним чином:

```Go
str := "Привіт, Світ!"
result := strings.ToLower(str)
fmt.Println(result)
```

Результатом буде "привіт, світ!". Також існує можливість використати цю функцію для конвертації окремих символів до нижнього регістру:

```Go
char := 'A'
result := strings.ToLower(string(char))
fmt.Println(result)
```

Результатом буде "a".

## Deep Dive
У Go існує два основних способи перетворення рядка до нижнього регістру. Перший - використання пакету strings, який ми показали в прикладах вище. Другий - використання Unicode і приведення кожного символу до нижнього регістру. Цей спосіб є більш ефективним для обробки великих обсягів даних.

## See Also
- [Офіційна документація Go](https://golang.org/pkg/strings/#ToLower)
- [Стаття про роботу з рядками у Go](https://www.digitalocean.com/community/tutorials/how-to-manipulate-strings-in-go-uk)
- [Приклади використання strings.ToLower в Go](https://www.dotnetperls.com/lower-go)