---
title:                "Escrevendo um arquivo de texto"
html_title:           "Elm: Escrevendo um arquivo de texto"
simple_title:         "Escrevendo um arquivo de texto"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/writing-a-text-file.md"
---

{{< edit_this_page >}}

## O que & Por quê?
Escrever um arquivo de texto é uma tarefa comum para programadores, pois é uma forma de armazenar informações importantes em um formato legível por humanos. Além disso, arquivos de texto são frequentemente utilizados para armazenar dados e configurações para serem acessados e manipulados por programas.

## Como fazer:
Para escrever um arquivo de texto em Elm, podemos usar a função `File.WriteString` que nos permite especificar o nome do arquivo e o conteúdo que queremos escrever. Veja o exemplo abaixo:

```Elm
import File

main =
    File.WriteString "arquivo.txt" "Olá mundo!"
```

Após executar este código, um novo arquivo de texto chamado "arquivo.txt" será criado com o conteúdo "Olá mundo!". Bem simples, não é?

## Mergulho profundo:
O conceito de escrever arquivos de texto tem sido utilizado pelos programadores por décadas. Nas linguagens de programação mais antigas, o processo era mais complicado e envolvia a utilização de bibliotecas externas. No entanto, com o surgimento de novas linguagens como Elm, esse processo se tornou mais simples e acessível.

Embora a escrita de arquivos de texto seja uma forma muito comum de armazenar informações, existem outras opções disponíveis, como bancos de dados e arquivos binários. No entanto, a escrita de arquivos de texto continua sendo uma das maneiras mais simples e práticas de armazenar dados simples.

## Veja também:
- Documentação oficial do Elm: https://elm-lang.org/docs
- Repositório do Elm no GitHub: https://github.com/elm-lang
- Tutorial sobre leitura e escrita de arquivos em Elm: https://guide.elm-lang.org/io/files.html