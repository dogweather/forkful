---
title:    "Go: Verificando se um diretório existe"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que verificar se um diretório existe: 

Uma parte importante da programação é garantir que seu código seja eficiente e capaz de lidar com diferentes situações. Uma dessas situações é verificar se um diretório existe antes de executar uma operação em cima dele. Isso pode ser especialmente útil em casos onde um programa precisa acessar um arquivo ou diretório específico no sistema do usuário.

## Como fazer:

Para verificar se um diretório existe em Go, podemos usar a função `Exists()` da biblioteca padrão `os`. Essa função verifica se um caminho (path) existe e retorna um valor booleano, indicando se é um diretório válido ou não.

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    dir := "/home/usuario/arquivos"
    
    exists := Exists(dir)
    
    if exists {
        fmt.Println("O diretório", dir, "existe!")
    } else {
        fmt.Println("O diretório", dir, "não existe.")
    }
}

func Exists(path string) bool {
    _, err := os.Stat(path)
    return !os.IsNotExist(err)
}
```

O código acima mostra um exemplo simples de como utilizar a função `Exists()` para verificar se um diretório existe em um determinado caminho. O resultado do programa será impresso na tela, informando se o diretório existe ou não.

## Aprofundando-se no assunto:

Para um entendimento mais profundo sobre a função `Exists()` e o processo de verificar se um diretório existe em Go, é importante entender como essa função funciona. A primeira linha dentro da função `Exists()` usa a função `Stat()` da biblioteca `os` para obter informações sobre o caminho especificado. Se o caminho não existir, a função `Stat()` retornará um erro, que pode ser checado pela função `IsNotExist()` da biblioteca `os`, que retorna um valor booleano indicando se o caminho não existe.

Caso queira verificar se um diretório existe sem precisar criar uma função separada, também é possível usar a função `os.Stat()` diretamente no código, como mostrado abaixo:

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    dir := "/home/usuario/arquivos"
    
    _, err := os.Stat(dir)
    
    if err == nil {
        fmt.Println("O diretório", dir, "existe!")
    } else {
        fmt.Println("O diretório", dir, "não existe.")
    }
}
```

Essa é uma forma mais simplificada de utilizar a função `os.Stat()` para verificar a existência de um diretório.

## Veja também:

- [Documentação oficial do pacote `os` em Go](https://golang.org/pkg/os/)
- [Tutorial: Como criar e verificar diretórios com Go](https://dev.to/dephraiim/how-to-create-and-check-if-directory-exists-on-local-system-with-packageos-golang-1a0g)
- [Exemplos de uso da função `Exists()`](https://golangcode.com/check-if-a-file-exists/)