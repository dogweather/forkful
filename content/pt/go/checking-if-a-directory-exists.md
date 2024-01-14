---
title:                "Go: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que verificar se um diretório existe?

Às vezes, ao escrever um programa em Go, é necessário verificar se um diretório existe antes de tentar acessar ou criar arquivos dentro dele. Isso ajuda a evitar erros e a garantir que o programa funcione corretamente.

## Como fazer

Para verificar se um diretório existe em Go, podemos usar a função nativa `os.Stat()` que retorna informações sobre um arquivo ou diretório. Dentro do pacote `os`, também temos a função `IsNotExist()` que verifica se um determinado erro é causado pela inexistência de um arquivo ou diretório.

```Go
info, err := os.Stat("caminho/do/diretorio")
if err != nil {
    if os.IsNotExist(err) {
        fmt.Println("O diretório não existe.")
    } else {
        fmt.Println("Erro:", err)
    }
} else {
    fmt.Println("O diretório existe.")
}
```

A saída deste código dependerá de se o diretório especificado existe ou não.

## Mergulho profundo

Além da função `os.Stat()` e `IsNotExist()`, podemos usar outras funções e métodos para trabalhar com a verificação de diretórios. Por exemplo, a função `MkdirAll()` cria um diretório e todos os subdiretórios necessários, e a função `os.RemoveAll()` remove um diretório e todos os seus subdiretórios.

```Go
err := os.MkdirAll("caminho/do/novo/diretorio", 0755)
if err != nil {
    fmt.Println("Erro ao criar o diretório:", err)
}

err := os.RemoveAll("caminho/do/diretorio")
if err != nil {
    fmt.Println("Erro ao remover o diretório:", err)
}
```

Além disso, existem pacotes externos como o `filepath` que oferece funções úteis para lidar com caminhos de arquivos e diretórios.

## Veja também

- [Documentação oficial do pacote `os`](https://golang.org/pkg/os/)
- [Pacote `filepath` na GoDocs](https://godoc.org/filepath)
- [Tutorial de Go na DevMedia](https://www.devmedia.com.br/introducao-a-linguagem-go-golang/28687)