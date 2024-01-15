---
title:                "Escrevendo um arquivo de texto"
html_title:           "Gleam: Escrevendo um arquivo de texto"
simple_title:         "Escrevendo um arquivo de texto"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por que escrever um arquivo de texto?

Escrever um arquivo de texto é uma tarefa comum para programadores, mas por que é tão importante? A resposta é simples: um arquivo de texto permite que você armazene e organize informações de maneira fácil e acessível. Seja para armazenar dados, gerar relatórios ou até mesmo criar documentos, a habilidade de escrever um arquivo de texto é essencial para qualquer programador.

## Como escrever um arquivo de texto no Gleam

Para escrever um arquivo de texto no Gleam, você pode usar a biblioteca interna "File", que fornece funções para criar, ler e editar arquivos de texto.

Para começar, primeiro devemos criar um arquivo de texto usando a função "create" da biblioteca "File". Por exemplo:

```Gleam
let file = File.create("meu_arquivo.txt")
```

Em seguida, podemos escrever conteúdo no arquivo usando a função "write" e, finalmente, salvar as alterações usando a função "flush". Veja um exemplo completo abaixo:

```Gleam
let file = File.create("meu_arquivo.txt")

File.write(file, "Olá, mundo!")
File.flush(file)
```

Isso irá criar um arquivo chamado "meu_arquivo.txt" e escrever a frase "Olá, mundo!" nele.

## Mergulho Profundo

Além das funções mencionadas acima, a biblioteca "File" também oferece outras funcionalidades úteis, como a possibilidade de ler e editar arquivos existentes, bem como definir permissões de acesso a eles.

Por exemplo, para ler o conteúdo de um arquivo, podemos usar a função "read" e especificar o número de bytes que desejamos ler:

```Gleam
let file = File.open("meu_arquivo.txt")
let conteudo = File.read(file, 10)
```

Este código irá ler os primeiros 10 bytes do arquivo e armazená-los na variável "conteudo".

Para alterar as permissões de um arquivo, podemos usar a função "set_permissions" e especificar as permissões desejadas, como leitura, escrita e execução.

## Veja também

- [Documentação oficial do Gleam](https://gleam.run/documentation/)
- [Exemplos de arquivos de texto no Gleam](https://github.com/gleam-lang/gleam/tree/main/examples/files)