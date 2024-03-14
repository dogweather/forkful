---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:58:26.296088-07:00
description: "A interpola\xE7\xE3o de strings \xE9 um m\xE9todo para construir strings\
  \ que incorporam vari\xE1veis, permitindo a cria\xE7\xE3o din\xE2mica de strings.\
  \ Programadores fazem isso\u2026"
lastmod: '2024-03-13T22:44:46.045541-06:00'
model: gpt-4-0125-preview
summary: "A interpola\xE7\xE3o de strings \xE9 um m\xE9todo para construir strings\
  \ que incorporam vari\xE1veis, permitindo a cria\xE7\xE3o din\xE2mica de strings.\
  \ Programadores fazem isso\u2026"
title: Interpolando uma String
---

{{< edit_this_page >}}

## O Quê & Porquê?

A interpolação de strings é um método para construir strings que incorporam variáveis, permitindo a criação dinâmica de strings. Programadores fazem isso para personalizar mensagens, construir URLs, criar consultas SQL e mais, permitindo um código mais legível e mantível.

## Como fazer:

No Go, a interpolação de strings é comumente alcançada usando o pacote `fmt`, particularmente com a função `Sprintf`, que permite injetar variáveis em uma string especificando verbos de formatação. Os verbos são placeholders na string de formato e são substituídos pelos valores das variáveis dadas. Aqui está como usá-la:

```go
package main

import (
    "fmt"
)

func main() {
    name := "Jane"
    age := 28

    // Usando Sprintf para interpolação de string
    message := fmt.Sprintf("Olá, meu nome é %s e eu tenho %d anos.", name, age)
    fmt.Println(message) // Saída: Olá, meu nome é Jane e eu tenho 28 anos.
}
```

Note que `%s` é usado para strings, e `%d` para inteiros. A documentação do pacote `fmt` fornece uma lista abrangente de verbos de formatação para diferentes tipos de dados.

## Aprofundamento

O conceito de interpolação de strings existe em muitas linguagens de programação, embora com diferentes sintaxes e capacidades. No Go, enquanto a função `Sprintf` do pacote `fmt` é a abordagem mais comumente usada, ela pode nem sempre ser a mais eficiente, especialmente para concatenações simples ou quando se trabalha dentro de código altamente sensível à performance.

O pacote `fmt` usa reflexão para interpretar dinamicamente os tipos das variáveis em tempo de execução, o que, embora flexível, acarreta sobrecarga. Para cenários onde a performance é crítica, a concatenação direta de strings ou o tipo `strings.Builder` podem oferecer alternativas melhores. A concatenação direta é simples, mas pode se tornar difícil de gerenciar com múltiplas variáveis. O `strings.Builder`, por outro lado, fornece uma maneira mais performática e legível de construir strings complexas em um loop ou quando se lida com muitas variáveis:

```go
var sb strings.Builder
sb.WriteString("Olá, meu nome é ")
sb.WriteString(name)
sb.WriteString(" e eu tenho ")
sb.WriteString(strconv.Itoa(age))
sb.WriteString(" anos.")
message := sb.String()

fmt.Println(message) // Produz o mesmo resultado que antes
```

Ultimamente, a escolha entre `fmt.Sprintf`, concatenação direta e `strings.Builder` depende dos requisitos específicos da sua aplicação, como a complexidade da string sendo construída e considerações de performance.
