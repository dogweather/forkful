---
title:                "Interpolando uma string"
date:                  2024-01-20T17:50:54.394818-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolando uma string"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/interpolating-a-string.md"
---

{{< edit_this_page >}}

## O que é & Porquê?
Interpolação de strings é o jeito de encaixar valores de variáveis diretamente dentro de uma string. Programadores fazem isso para construir strings de maneira dinâmica e legível, facilitando a inclusão de dados variáveis em mensagens, logs ou qualquer outra saída de texto.

## Como fazer:
```Go
package main

import (
	"fmt"
)

func main() {
	nome := "José"
	idade := 29
	// Interpolação usando Sprintf
	mensagem := fmt.Sprintf("Olá, meu nome é %s e eu tenho %d anos.", nome, idade)
	fmt.Println(mensagem)
}
```
Saída da amostra:
```
Olá, meu nome é José e eu tenho 29 anos.
```

## Mergulho Profundo
Antes do Go, as linguagens como PHP e Ruby já suportavam interpolação de strings de uma forma ainda mais direta. Go não tem interpolação de string incorporada de forma pura, mas a função `fmt.Sprintf` é a ferramenta padrão para isso.

Alternativas incluem a concatenação direta com o operador `+`, mas isso pode ser verboso e menos eficiente. Outra opção é o uso de `strings.Builder` para montagens complexas de strings, que é mais eficiente para strings com múltiplas operações.

Na implementação, `fmt.Sprintf` faz uso de verbos de formatação (`%s` para strings, `%d` para números inteiros, etc.) que direcionam como a variável será formatada na string final. Isso dá flexibilidade e controle ao programador sobre a apresentação dos dados.

## Veja Também
- Documentação oficial do pacote `fmt`: https://pkg.go.dev/fmt
- Artigo sobre `strings.Builder`: https://pkg.go.dev/strings#Builder
- Tutorial sobre `fmt.Sprintf`: https://gobyexample.com/string-formatting
