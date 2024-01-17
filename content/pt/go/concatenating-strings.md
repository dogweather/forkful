---
title:                "Juntando sequências de caracteres"
html_title:           "Go: Juntando sequências de caracteres"
simple_title:         "Juntando sequências de caracteres"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## O que e por que? 

Concatenar strings em programação significa combinar duas ou mais strings de texto em uma única string. Os programadores geralmente fazem isso para criar mensagens personalizadas para usuários, construir URLs dinâmicos ou criar saídas formatadas. 

## Como fazer: 

Em Go, você pode concatenar strings usando o operador + ou a função strings.Join(). Veja alguns exemplos abaixo: 

```
// Usando o operador + 
package main 

import "fmt"

func main() {
   string1 := "Olá"
   string2 := "mundo!"
   fmt.Println(string1 + " " + string2) // Saída: Olá mundo!
}
```

```
// Usando a função strings.Join()
package main 

import "fmt"
import "strings"

func main() {
   palavras := []string{"Olá", "mundo!"}
   fmt.Println(strings.Join(palavras, " ")) // Saída: Olá mundo!
}
```

## Profundando:

Historicamente, a concatenação de strings era considerada uma operação cara em linguagens de programação. No entanto, em Go, as strings são tratadas como um tipo de dados imutável, o que facilita a otimização da concatenação de strings. Além disso, existem outras opções para concatenar strings, como o método fmt.Sprintf(), que formata uma string com valores variáveis. 

## Veja Também: 

- [Documentação oficial do Go sobre strings](https://golang.org/pkg/strings/)
- [Um tutorial em vídeo sobre como concatenar strings em Go](https://www.youtube.com/watch?v=P7dkMlifYKc)
- [Uma discussão sobre a eficiência da concatenação de strings em Go](https://medium.com/just-javascript/string-concatenation-pitfalls-in-go-and-javascript-e715ff419b44)