---
title:                "Работа с YAML"
date:                  2024-01-29T00:05:13.028496-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/go/working-with-yaml.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?
Работа с YAML подразумевает анализ и генерацию данных в формате YAML, что является стандартом сериализации данных, удобным для восприятия человеком. Программисты делают это для управления файлами конфигурации, обмена данными между языками и структурирования сложных данных.

## Как это сделать:
Чтобы работать с YAML в Go, вам понадобится библиотека, например, `gopkg.in/yaml.v3`. Установите её, используя:

```bash
go get gopkg.in/yaml.v3
```

Вот как анализировать YAML:

```Go
package main

import (
	"fmt"
	"log"
	"gopkg.in/yaml.v3"
)

var data = `
a: Легко!
b:
  c: 2
  d: [3, 4]
`

type StructA struct {
	A string
	B StructB
}

type StructB struct {
	C int
	D []int
}

func main() {
	var s StructA

	err := yaml.Unmarshal([]byte(data), &s)
	if err != nil {
		log.Fatalf("ошибка: %v", err)
	}
	fmt.Println(s)
}
```

Вывод:

```
{Легко! {2 [3 4]}}
```

Генерация YAML:

```Go
package main

import (
	"fmt"
	"gopkg.in/yaml.v3"
)

func main() {
	data := StructA{
		A: "Легко!",
		B: StructB{
			C: 2,
			D: []int{3, 4},
		},
	}

	d, err := yaml.Marshal(&data)
	if err != nil {
		log.Fatalf("ошибка: %v", err)
	}
	fmt.Printf("---\n%s\n", string(d))
}
```

Вывод:

```
---
a: Легко!
b:
  c: 2
  d:
  - 3
  - 4
```

## Подробно
YAML был создан в 2001 году с целью быть удобным для человека форматом обмена данными. Он используется как альтернатива JSON и XML, поскольку он более читаемый и может представлять сложные структуры данных. В Go нет встроенной поддержки YAML, поэтому популярны сторонние библиотеки, такие как `gopkg.in/yaml.v3`. Библиотека использует libyaml, парсер и эмиттер YAML на C, для эффективности и соответствия стандартам YAML.

## Смотрите также
- Документация пакета YAML v3: https://pkg.go.dev/gopkg.in/yaml.v3
- Официальный сайт YAML: https://yaml.org
- Спецификация YAML: https://yaml.org/spec/1.2/spec.html
- Онлайн-конвертер из JSON в YAML: https://www.json2yaml.com/
