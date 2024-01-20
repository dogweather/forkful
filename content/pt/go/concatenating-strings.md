---
title:                "Concatenando strings"
html_title:           "Elixir: Concatenando strings"
simple_title:         "Concatenando strings"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Concatenar strings significa unir duas ou mais strings para criar uma nova. Programadores fazem isso para manipular dados textuais, formatar saídas e construir consultas dinâmicas.

## Como Fazer:
Em Go, você pode usar o operador `+` ou a função `fmt.Sprintf` para concatenar strings. Aqui estão exemplos de ambos:

```Go
package main
import "fmt"

func main() {
    var str1 = "Olá, "
    var str2 = "mundo!"
    fmt.Println(str1 + str2)  // Saída: Olá, mundo!
}
```

E usando `fmt.Sprintf`:

```Go
package main
import "fmt"

func main() {
    var str1 = "Olá, "
    var str2 = "mundo!"
    result := fmt.Sprintf("%s%s", str1, str2)
    fmt.Println(result)  // Saída: Olá, mundo!
}
```

## Mergulho Profundo:
A concatenação de strings tem uma longa história em programação desde os primeiros dias do Fortran. Em Go, ela é implementada usando recursos internos do runtime do Go e otimizada para eficiência.

Alternativamente, para concatenar strings em grandes volumes, você pode usar `strings.Builder`. Esse método é mais eficiente em termos de memória e desempenho, pois evita a criação de muitos objetos de string temporários.

```Go
package main
import (
    "strings"
    "fmt"
)

func main() {
    var builder strings.Builder
    builder.WriteString("Olá, ")
    builder.WriteString("mundo!")
    fmt.Println(builder.String())  // Saída: Olá, mundo!
}
```

## Veja Também:
1. Documentação oficial do Go: [Package strings](https://golang.org/pkg/strings/)
2. Artigo relacionado: [Efficient String Concatenation in Go](https://hermanschaaf.com/efficient-string-concatenation-in-go/)
3. Como concatenar strings eficientemente: [Go: How to Concatenate Strings Efficiently](https://levelup.gitconnected.com/go-how-to-concatenate-strings-efficiently-4b0334ea7ef1)