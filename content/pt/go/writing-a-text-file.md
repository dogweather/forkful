---
title:    "Go: Escrevendo um arquivo de texto"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/go/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por que

Escrever arquivos de texto é um aspecto importante da programação em Go. Isso permite que os desenvolvedores armazenem e manipulem dados em um formato simples e legível para humanos. Além disso, criar e editar arquivos de texto é uma habilidade útil que pode ser aplicada em vários projetos.

## Como fazer

Para escrever um arquivo de texto em Go, vamos seguir estas etapas:

1. Importe o pacote "os", que fornece funcionalidades para lidar com o sistema operacional.
2. Crie um arquivo usando a função "Create" do pacote "os" e atribua-o a uma variável.
3. Use a função "WriteString" para escrever uma string no arquivo recém-criado.
4. Feche o arquivo usando a função "Close".

Aqui está um exemplo de código que cria um arquivo de texto chamado "meuarquivo.txt" e escreve a string "Olá, mundo!" nele:

```
import "os"

arquivo, err := os.Create("meuarquivo.txt")

if err != nil {
    // lida com erros, se houver algum
}

defer arquivo.Close()

_, err = arquivo.WriteString("Olá, mundo!")

if err != nil {
    // lida com erros, se houver algum
}
```

O operador "defer" garante que o arquivo seja fechado após a execução da função "WriteString", mesmo se houver algum erro. Além disso, o uso do underscore (_) antes da variável "erro" ignora seu valor, indicando que não precisamos fazer nada com ele neste caso.

Quando o código é executado, ele cria um novo arquivo de texto com a string "Olá, mundo!" dentro dele.

## Aprofundando

Há muito mais para aprender sobre como escrever arquivos de texto em Go. Você pode aprender sobre as diferentes opções de abertura de arquivos, como usar o pacote "bufio" para leitura mais eficiente de arquivos grandes e como lidar com erros ao escrever em arquivos. É importante também se familiarizar com os tipos de dados usados em Go, como strings e bytes, e como eles podem ser usados ​​para escrever arquivos de texto.

## Veja também

Aqui estão alguns links úteis para aprender mais sobre como escrever arquivos de texto em Go:

- [Documentação oficial do pacote "os"](https://golang.org/pkg/os/)
- [Aprenda Go com testes - Escrevendo arquivos](https://quii.gitbook.io/learn-go-with-tests/go-fundamentals/writing-files)
- [Guia de estudo Go - Escrevendo arquivos](https://github.com/ispyhumanfly/gopherguides-learn-studygroup/blob/master/howtos/introtoGo/docs/writing_files_in_go.md)