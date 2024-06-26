---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:12:06.202398-07:00
description: "Como fazer: Em Go, o pacote `encoding/json` \xE9 o seu portal para manipula\xE7\
  \xE3o de JSON, fornecendo mecanismos para converter estruturas de dados de Go para\u2026"
lastmod: '2024-03-13T22:44:46.083950-06:00'
model: gpt-4-0125-preview
summary: "Em Go, o pacote `encoding/json` \xE9 o seu portal para manipula\xE7\xE3\
  o de JSON, fornecendo mecanismos para converter estruturas de dados de Go para JSON\
  \ (marshalling) e vice-versa (unmarshalling)."
title: Trabalhando com JSON
weight: 38
---

## Como fazer:
Em Go, o pacote `encoding/json` é o seu portal para manipulação de JSON, fornecendo mecanismos para converter estruturas de dados de Go para JSON (marshalling) e vice-versa (unmarshalling). Abaixo estão exemplos básicos para começar:

### Codificação (Marshalling)
Para converter uma estrutura Go para JSON, você pode usar `json.Marshal`. Considere a seguinte estrutura Go:

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

Saída:

```json
{"id":1,"username":"JohnDoe","languages":["Go","JavaScript","Python"]}
```

### Decodificação (Unmarshalling)
Para analisar JSON em uma estrutura de dados de Go, use `json.Unmarshal`:

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

Dada a struct `User` como antes, esse código analisa a string JSON em uma instância de User.

Saída:

```go
{ID:1 Username:JohnDoe Languages:[Go JavaScript Python]}
```

## Aprofundamento
O pacote `encoding/json` em Go oferece uma API direta que abstrai grande parte da complexidade envolvida na manipulação de JSON. Introduzido no início do desenvolvimento do Go, esse pacote reflete a filosofia de simplicidade e eficiência do Go. No entanto, o uso de reflexão pelo `encoding/json` para inspecionar e modificar structs em tempo de execução pode levar a desempenho menos que ótimo em cenários intensivos de CPU.

Alternativas como `json-iterator/go` e `ffjson` surgiram, fornecendo processamento de JSON mais rápido ao gerar código de marshalling e unmarshalling estático. No entanto, `encoding/json` permanece como o pacote mais usado devido à sua simplicidade, robustez e o fato de ser parte da biblioteca padrão, garantindo compatibilidade e estabilidade através das versões do Go.

Apesar de seu desempenho relativamente mais lento, a facilidade de uso e a integração com o sistema de tipos do Go tornam `encoding/json` adequado para a maioria das aplicações. Para aqueles que trabalham em contextos onde o desempenho é primordial, explorar bibliotecas externas pode ser valioso, mas para muitos, a biblioteca padrão oferece o equilíbrio certo entre velocidade, simplicidade e confiabilidade.
