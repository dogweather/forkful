---
title:                "Deletando caracteres que correspondem a um padrão"
aliases:
- /pt/go/deleting-characters-matching-a-pattern.md
date:                  2024-02-03T17:55:36.220955-07:00
model:                 gpt-4-0125-preview
simple_title:         "Deletando caracteres que correspondem a um padrão"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/deleting-characters-matching-a-pattern.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Que?

Deletar caracteres que correspondem a um padrão específico é sobre remover certos caracteres ou sequências de caracteres de strings, com base em regras definidas por um padrão (geralmente via expressões regulares). Programadores frequentemente precisam realizar essa tarefa para limpeza de dados, pré-processamento para análise, formatação de saída ou simplesmente manipulando strings para atender aos requisitos da aplicação.

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
