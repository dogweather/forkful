---
title:    "Go: Escrevendo um arquivo de texto"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Porquê

Escrever arquivos de texto é uma tarefa comum na programação. Seja para armazenar dados, criar logs ou gerar relatórios, saber como escrever um arquivo de texto é uma habilidade importante para qualquer programador Go.

## Como fazer

A linguagem Go possui uma biblioteca padrão completa para manipulação de arquivos. Para escrever um arquivo de texto, primeiro precisamos abrir um arquivo com a função `os.Create()`, que retorna um ponteiro para o arquivo.

```
file, err := os.Create("arquivo.txt")

if err != nil {
    fmt.Println("Erro ao criar arquivo:", err)
    return
}
```

Em seguida, podemos usar o ponteiro para escrever no arquivo usando a função `fmt.Fprintf()`. Esta função funciona de maneira semelhante ao `fmt.Printf()`, onde podemos passar uma string de formatação e os valores a serem substituídos na string.

```
_, err = fmt.Fprintf(file, "Olá, mundo! Este é um arquivo de texto escrito em Go." )

if err != nil {
    fmt.Println("Erro ao escrever no arquivo:", err)
    return
}
```

Por fim, é importante fechar o arquivo usando a função `file.Close()`, para liberar recursos e garantir que todas as alterações sejam salvas.

```
err = file.Close()

if err != nil {
    fmt.Println("Erro ao fechar arquivo:", err)
    return
}

fmt.Println("Arquivo criado e escrito com sucesso!")
```

## Mergulho Profundo

Além da função `os.Create`, a biblioteca padrão do Go também possui outras funções úteis para escrita de arquivos de texto, como `os.OpenFile()`, que permite abrir um arquivo já existente, e `ioutil.WriteFile()`, que fornece um método simples para escrever todo o conteúdo de uma string em um arquivo.

É importante lembrar que ao escrever em um arquivo, devemos sempre verificar e lidar com possíveis erros, para garantir que nosso programa seja robusto e confiável.

## Veja também

- [Documentação oficial do pacote `os`](https://golang.org/pkg/os/)
- [Tutorial sobre manipulação de arquivos no Go](https://gobyexample.com/writing-files)
- [Exemplos de escrita de arquivos em Go](https://golangcode.com/write-write-string-to-a-text-file/)