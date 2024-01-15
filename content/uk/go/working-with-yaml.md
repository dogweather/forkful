---
title:                "Робота з yaml"
html_title:           "Go: Робота з yaml"
simple_title:         "Робота з yaml"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/working-with-yaml.md"
---

{{< edit_this_page >}}

## Чому

Використання YAML є дуже корисним і простим для розуміння способом зберігання та обробки структурованої інформації. Цей формат є популярним серед розробників програмного забезпечення та дозволяє зручно керувати налаштуваннями та даними.

## Як

Для початку роботи з YAML потрібно встановити бібліотеку `gopkg.in/yaml.v2` і імпортувати її у свій проект:

```
import "gopkg.in/yaml.v2"
```

Створюємо структуру даних в Go та заповнюємо її значеннями:

```
type Person struct {
    Name string
    Age  int
    Job  string
}

p := Person{Name: "John", Age: 25, Job: "Developer"}
```

Далі, для перетворення цієї структури в YAML формат потрібно використати функцію `Marshal` та передати в неї структуру даних у вигляді `[]byte`:

```
yamlData, err := yaml.Marshal(p)
if err != nil {
    log.Fatalf("unable to marshal data: %v", err)
}
```

Тепер перетворений YAML код може бути виведений, наприклад, на екран:

```
fmt.Println(string(yamlData))
```

Результат виглядатиме наступним чином:

```
name: John
age: 25
job: Developer
```

## Deep Dive

При роботі з YAML важливо звернути увагу на правильну структуру файлу. Він повинен бути добре структурований та відформатований, щоб уникнути помилок при обробці даних.

Крім того, бібліотека `yaml.v2` вміє працювати не тільки зі структурами даних, а й зі звичайними типами, такими як `string`, `int` чи `bool`. При цьому важливо правильно вказати тип даних під час використання функції `Marshal`:

```
b, err := yaml.Marshal(struct {
	Number int    `yaml:"number"`
	Msg    string `yaml:"message"`
}{Number: 1, Msg: "Hello!"})

fmt.Println(string(b))
```

Результат буде таким:

```
number: 1
message: Hello!
```

## Дивись також

- [YAML офіційний сайт](https://yaml.org/)
- [Програмування на Go](https://go.dev/)