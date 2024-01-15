---
title:                "Робота з json"
html_title:           "Go: Робота з json"
simple_title:         "Робота з json"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/working-with-json.md"
---

{{< edit_this_page >}}

##Чому

Робота з JSON є невід'ємною частиною розробки веб-додатків і додатків для мобільних пристроїв. Використання мови програмування Go для роботи з цим форматом данних дозволяє легко та ефективно обробляти та обмінюватись даними між різними додатками та системами.

##Як

Найпростіший спосіб працювати з JSON в Go - використання пакету "encoding/json" для преобразування даних в форматі JSON в об'єкти типу struct та навпаки. Нижче наведений приклад коду, який демонструє цей підхід:

```Go
package main

import (
	"encoding/json"
	"fmt"
)

type Person struct {
	Name    string `json:"name"`
	Age     int    `json:"age"`
	Hobbies []string `json:"hobbies"`
}

func main() {
	jsonData := `{"name": "John", "age": 30, "hobbies": ["reading", "running"]}`

	var person Person

	err := json.Unmarshal([]byte(jsonData), &person)
	if err != nil {
		fmt.Println("Error parsing JSON:", err)
	}

	fmt.Println("Name:", person.Name)
	fmt.Println("Age:", person.Age)
	fmt.Println("Hobbies:", person.Hobbies)
}
```

При запуску цього коду буде виведено наступний результат:

```bash
Name: John
Age: 30
Hobbies: [reading running]
```

Щоб згенерувати JSON з об'єктів типу struct, можна використовувати метод "Marshal" пакету "encoding/json". Наступний приклад коду демонструє цей підхід:

```Go
package main

import (
	"encoding/json"
	"fmt"
)

type Person struct {
	Name    string `json:"name"`
	Age     int    `json:"age"`
	Hobbies []string `json:"hobbies"`
}

func main() {
	person := Person{Name: "Mary", Age: 25, Hobbies: []string{"painting", "hiking"}}

	result, err := json.Marshal(person)
	if err != nil {
		fmt.Println("Error encoding JSON:", err)
	}

	fmt.Println(string(result))
}
```

Після запуску цього коду буде виведений наступний результат:

```bash
{"name":"Mary","age":25,"hobbies":["painting","hiking"]}
```

##Глибокий занурення

У мові програмування Go є багато інших способів роботи з JSON. Наприклад, пакет "encoding/json" дозволяє задавати налаштування для кодування та декодування JSON з використанням тегів структури. Також є можливість використовувати інші пакети, такі як "jsoniter", для більш швидкої та ефективної роботи з JSON.

##Дивіться також

- [Офіційна документація Go для пакету "encoding/json"](https://golang.org/pkg/encoding/json/)
- [Довідкова стаття про роботу з JSON в Go на сайті Medium](https://medium.com/@arslando/json-processing-in-go-part-i-3b3fc32a8600)
- [Стаття про роботу з JSON у мові програмування Go на сайті dev.to](https://dev.to/wagslane/working-with-json-in-go-is-a-breeze-4j4e)