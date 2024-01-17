---
title:                "Imprimindo saída de depuração"
html_title:           "Go: Imprimindo saída de depuração"
simple_title:         "Imprimindo saída de depuração"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## O que é e por que fazer?

"Impressão de debug output" é um termo utilizado pelos programadores para se referir à prática de exibir informações de depuração durante a execução de um programa. Isso pode incluir informações como valores de variáveis, mensagens de erro ou outras informações úteis que possam ajudar a entender e solucionar problemas no código. Os programadores fazem isso para facilitar a identificação e correção de erros em seus programas, tornando o processo de desenvolvimento mais eficiente e eficaz.

## Como fazer:

```Go
// Exemplo de imprimir informações de depuração
package main

import "fmt"

func main() {
    var num1, num2 int = 5, 10
    fmt.Println("Iniciando programa...")
    fmt.Println("Primeiro número:", num1)
    fmt.Println("Segundo número:", num2)
    fmt.Println("Resultado da soma", num1 + num2)
}
```

Ao executar este programa, a saída seria:

```
Iniciando programa...
Primeiro número: 5
Segundo número: 10
Resultado da soma: 15
```

## Profundando:

A impressão de debug output tem sido uma prática comum entre programadores desde os primeiros dias da programação. Antigamente, os programadores utilizavam técnicas como saídas em papel para identificar problemas em seus códigos, mas com o avanço da tecnologia, tornou-se possível imprimir as informações diretamente no console do computador. Embora seja uma técnica útil, é importante notar que existem outras formas de depuração, como o uso de ferramentas de debug específicas. No caso de Go, a própria linguagem oferece uma biblioteca de depuração chamada "log", que pode ser utilizada para exibir informações de debug.

## Veja também:

- Documentação oficial do pacote de debug da linguagem Go: https://golang.org/pkg/log/
- Tutorial completo sobre como fazer impressão de debug output em Go: https://aprenda-golang.space/channels/debugging-output/
- Vídeo explicando diferentes formas de depuração em Go: https://www.youtube.com/watch?v=Y-9epIwG8cM