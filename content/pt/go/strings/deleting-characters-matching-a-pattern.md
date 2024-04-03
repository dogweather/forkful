---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:55:36.220955-07:00
description: "Deletar caracteres que correspondem a um padr\xE3o espec\xEDfico \xE9\
  \ sobre remover certos caracteres ou sequ\xEAncias de caracteres de strings, com\
  \ base em regras\u2026"
lastmod: '2024-03-13T22:44:46.043384-06:00'
model: gpt-4-0125-preview
summary: "Deletar caracteres que correspondem a um padr\xE3o espec\xEDfico \xE9 sobre\
  \ remover certos caracteres ou sequ\xEAncias de caracteres de strings, com base\
  \ em regras definidas por um padr\xE3o (geralmente via express\xF5es regulares)."
title: "Deletando caracteres que correspondem a um padr\xE3o"
weight: 5
---

## Como fazer:
Em Go, deletar caracteres que correspondem a um padrão pode ser eficientemente realizado usando o pacote `regexp`. Aqui, vamos mostrar como remover todos os dígitos, depois todos os caracteres não alfanuméricos de uma string como exemplos.

1. **Removendo Todos os Dígitos:**

```go
package main

import (
    "fmt"
    "regexp"
)

func main() {
    text := "Go1 é legal, mas Go2 será mais legal! Agora: 2023."
	
    // Compila a expressão regular para dígitos
    re, err := regexp.Compile("[0-9]+")
    if err != nil {
        fmt.Println("Erro ao compilar regex:", err)
        return
    }
	
    // Substitui dígitos por uma string vazia
    result := re.ReplaceAllString(text, "")
	
    fmt.Println(result) // Saída: Go é legal, mas Go será mais legal! Agora: .
}
```

2. **Removendo Todos os Caracteres Não Alfanuméricos:**

```go
package main

import (
    "fmt"
    "regexp"
)

func main() {
    text := "Go é #1 @ linguagens de programação!"
	
    // Compila a expressão regular para caracteres não alfanuméricos
    re, err := regexp.Compile("[^a-zA-Z0-9]+")
    if err != nil {
        fmt.Println("Erro ao compilar regex:", err)
        return
    }
	
    // Substitui caracteres não alfanuméricos por uma string vazia
    result := re.ReplaceAllString(text, "")
	
    fmt.Println(result) // Saída: Goé1linguagensdeprogramação
}
```

## Aprofundamento
O pacote `regexp` em Go fornece uma interface poderosa para correspondência de padrões e manipulação com expressões regulares. Sua implementação é derivada do RE2, uma biblioteca de expressão regular projetada para garantir uma execução em tempo linear, evitando a possibilidade de problemas de "retrocessos catastróficos" presentes em alguns outros motores de regex. Isso torna as regex de Go relativamente seguras e eficientes para uma ampla gama de aplicações.

Embora o pacote `regexp` seja uma solução abrangente para lidar com padrões, vale ressaltar que para manipulações de strings mais simples ou altamente específicas, outras funções de string como `strings.Replace()`, `strings.Trim()`, ou fatiamento podem oferecer alternativas mais performáticas. Expressões regulares são uma ferramenta poderosa, mas seu relativo custo computacional significa que para operações que podem ser especificadas sem elas, explorar alternativas da biblioteca padrão às vezes pode levar a um código mais simples e eficiente.
