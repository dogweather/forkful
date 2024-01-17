---
title:                "З'єднання рядків"
html_title:           "Go: З'єднання рядків"
simple_title:         "З'єднання рядків"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## Що & Чому?
Конкатенація рядків - це процес об'єднання двох або більше рядків в один. Програмісти часто використовують цей метод для створення нових рядків з вже існуючих або для додавання потрібних даних до існуючого рядка.

## Як це зробити:
Щоб здійснити конкатенацію рядків у Go, вам потрібно використовувати оператор "+" або "fmt.Sprintf" функцію. Давайте розглянемо кілька прикладів:

```Go
package main

import "fmt"

func main() {
    // Конкатенація металевих жанрів
    metal := "Thrash" + " " + "Metal"
    fmt.Println(metal) // Виведе "Thrash Metal"

    // Конкатенація чисел
    num1 := "1"
    num2 := "2"
    sum := num1 + num2
    fmt.Println(sum) // Виведе "12"

    // Конкатенація рядків за допомогою fmt.Sprintf()
    text1 := "Програмування"
    text2 := "це"
    text3 := "круто!"
    sentence := fmt.Sprintf("%s %s %s", text1, text2, text3)
    fmt.Println(sentence) // Виведе "Програмування це круто!"
}
```

## Глибші занурення:
Конкатенація рядків була вперше використана в мові програмування BCPL у 1960-х роках. У багатьох мовах програмування, таких як Python та JavaScript, існують спеціальні методи для об'єднання рядків. Однак, у Go існує декілька оптимізаційних підходів, що дозволяють робити конкатенацію більш ефективно і швидко. Наприклад, Go застосовує оптимізацію пам'яті, коли ви користуєтесь оператором "+". Також існує функція "strings.Join()", яка може бути більш ефективною для конкатенації багатьох рядків.

## Додаткові ресурси:
Якщо ви бажаєте дізнатися більше про конкатенацію рядків у Go, перегляньте наступні посилання:
- [Документація Go на конкатенацію рядків](https://golang.org/doc/effective_go.html#string_concatenation)
- [Приклади використання "strings.Join()" у Go](https://www.digitalocean.com/community/tutorials/how-to-join-strings-in-go)
- [Стаття про оптимізацію конкатенації у Go](https://medium.com/a-journey-with-go/go-making-the-string-concatenation-faster-continuous-allocation-assemblies-9300b33c1a57)