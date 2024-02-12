---
title:                "Робота з JSON"
date:                  2024-02-03T18:12:43.896099-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що та Чому?

Робота з JSON (JavaScript Object Notation) в Go включає кодування та декодування даних між структурами даних Go та форматом JSON. Це завдання є повсюдним у веб-сервісах та API, оскільки JSON служить легковісним, текстовим та незалежним від мови форматом обміну даними, що дозволяє просто обмінюватися даними між різними програмними середовищами.

## Як це зробити:

У Go пакет `encoding/json` є вашими воротами до маніпуляцій з JSON, надаючи механізми для конвертації структур даних Go в JSON (маршалінг) та назад (анмаршалінг). Нижче наведені базові приклади для початку:

### Кодування (Маршалінг)

Для конвертації структури Go в JSON можна використовувати `json.Marshal`. Розглянемо наступну структуру Go:

```go
package main

import (
    "encoding/json"
    "fmt"
    "log"
)

type User struct {
    ID        int      `json:"id"`
    Username  string   `json:"username"`
    Languages []string `json:"languages"`
}

func main() {
    user := User{1, "JohnDoe", []string{"Go", "JavaScript", "Python"}}
    userJSON, err := json.Marshal(user)
    if err != nil {
        log.Fatal(err)
    }
    fmt.Println(string(userJSON))
}
```

Вивід:

```json
{"id":1,"username":"JohnDoe","languages":["Go","JavaScript","Python"]}
```

### Декодування (Анмаршалінг)

Для розбору JSON у структуру даних Go використовуйте `json.Unmarshal`:

```go
package main

import (
    "encoding/json"
    "fmt"
    "log"
)

func main() {
    jsonStr := `{"id":1,"username":"JohnDoe","languages":["Go","JavaScript","Python"]}`
    var user User
    err := json.Unmarshal([]byte(jsonStr), &user)
    if err != nil {
        log.Fatal(err)
    }
    fmt.Printf("%+v\n", user)
}
```

Враховуючи структуру `User` як раніше, цей код аналізує JSON рядок у інстанс User.

Вивід:

```go
{ID:1 Username:JohnDoe Languages:[Go JavaScript Python]}
```

## Поглиблене вивчення

Пакет `encoding/json` в Go пропонує простий API, який абстрагує багато складностей, пов'язаних з маніпуляціями JSON. Представлений на ранньому етапі розвитку Go, цей пакет відображає філософію Go простоти та ефективності. Однак використання рефлексії `encoding/json` для інспектування та модифікації структур в рантаймі може призвести до не найкращої продуктивності в CPU-інтенсивних сценаріях.

З'явилися альтернативи, такі як `json-iterator/go` та `ffjson`, які надають швидшу обробку JSON за рахунок генерації статичного коду маршалінгу та анмаршалінгу. Однак `encoding/json` залишається найбільш використовуваним пакетом через його простоту, надійність та той факт, що він є частиною стандартної бібліотеки, забезпечуючи сумісність і стабільність між версіями Go.

Незважаючи на його відносно повільну продуктивність, простота використання та інтеграція з системою типів Go роблять `encoding/json` підходящим для більшості застосунків. Для тих, хто працює в контекстах, де продуктивність є першочерговою, дослідження зовнішніх бібліотек може бути варте того, але для багатьох стандартна бібліотека знаходить правильний баланс між швидкістю, простотою та надійністю.
