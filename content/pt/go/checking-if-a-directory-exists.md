---
title:                "Verificando se um diretório existe"
html_title:           "Go: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que

Você pode estar se perguntando por que é importante verificar se um diretório existe em seu programa Go. A resposta é simples: ao verificar a existência de um diretório, você pode garantir que seu programa funcione corretamente e evitar erros e falhas inesperadas.

## Como fazer

Para verificar se um diretório existe em Go, podemos usar a função `os.Stat()`. Esta função retorna um objeto `FileInfo` contendo informações sobre um arquivo ou diretório especificado. Se o objeto retornado não for `nil`, isso significa que o diretório existe.

Vamos dar uma olhada em um exemplo de código que usa `os.Stat()` para verificar a existência de um diretório:

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    // Definir o caminho do diretório que queremos verificar
    dir := "/Users/meu_usuario/Documentos"

    // Verificar se o diretório existe
    if _, err := os.Stat(dir); err == nil {
        fmt.Println("O diretório existe!")
    } else if os.IsNotExist(err) {
        fmt.Println("O diretório não existe.")
    } else {
        fmt.Println("Ocorreu um erro ao verificar o diretório.")
    }
}
```

Neste exemplo, definimos uma variável `dir` contendo o caminho do diretório que desejamos verificar. Em seguida, usamos a função `os.Stat()` para obter as informações do diretório e, usando a declaração `if-else`, verificamos se o objeto retornado é `nil`. Se não for, isso significa que o diretório existe e podemos executar a lógica desejada. Se for `nil`, verificamos se o erro retornado indica que o diretório não existe ou se houve algum outro erro durante a verificação.

Ao executar este código, a saída será:

```
O diretório existe!
```

## Deep Dive

A função `os.Stat()` é apenas uma das várias maneiras de verificar a existência de um diretório em Go. Algumas outras funções úteis incluem `os.IsExist()`, que verifica se um arquivo ou diretório existe e retorna um booleano, e `filepath.Dir()`, que extrai o diretório de um caminho de arquivo.

No entanto, é importante mencionar que, ao verificar a existência de um diretório, também devemos estar cientes das permissões de arquivo. Por exemplo, se não tivermos permissões para acessar o diretório que estamos verificando, isso também será considerado um erro e a função `os.Stat()` retornará um objeto de erro.

Portanto, ao usar a função `os.Stat()`, devemos estar cientes das permissões de arquivo e tratar os possíveis erros adequadamente em nossa lógica de programação.

## Veja também

- [Documentação oficial do pacote os](https://golang.org/pkg/os/)
- [Artigo "Comandos básicos do sistema de arquivos em Go"](https://gobyexample.com/file-paths)