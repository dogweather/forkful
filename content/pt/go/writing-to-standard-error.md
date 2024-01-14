---
title:    "Go: Escrevendo no erro padrão"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Por que escrever para o erro padrão em Go?

Escrever para o erro padrão é uma importante prática de programação em Go que permite aos desenvolvedores identificar e solucionar erros em seus códigos de maneira eficiente. Além disso, também é útil para fornecer informações úteis e mensagens de erro mais claras aos usuários do seu programa.

## Como fazer

Para escrever para o erro padrão em Go, você deve usar a função `Println()` da biblioteca `fmt`. Essa função permite imprimir uma string no console, que será exibida como um erro pelo compilador ou pela execução do programa.

```
package main

import "fmt"

func main() {
    fmt.Println("Este é um erro padrão em Go!")
}
```

O código acima resultará na seguinte saída:

```
Este é um erro padrão em Go!
```

## Mergulho Profundo

Além da função `Println()`, a biblioteca `fmt` também possui outras funções úteis para escrever para o erro padrão, como `Printf()` e `Fprintln()`. Além disso, é possível também usar a biblioteca `log` para ter uma maior personalização e controle sobre os erros.

Porém, é importante lembrar que escrever para o erro padrão deve ser feito apenas para depurar e testar seu código, e não deve ser usado como uma solução permanente para problemas em seu programa.

## Veja também

- Documentação oficial do `fmt` em Go: https://golang.org/pkg/fmt/
- Tutorial de escrita para o erro padrão em Go: https://www.digitalocean.com/community/tutorials/how-to-write-to-standard-error-in-go
- Guia completo de boas práticas em Go: https://blog.golang.org/first-class-functions