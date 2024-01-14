---
title:    "Gleam: Escrevendo um arquivo de texto"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por que escrever um arquivo de texto?

Escrever um arquivo de texto pode ser uma tarefa simples, mas é uma parte importante do desenvolvimento de software. Com o Gleam, é possível criar facilmente arquivos de texto e utilizá-los em seus projetos.

## Como fazer

Para escrever um arquivo de texto em Gleam, primeiro importe o módulo "File" e, em seguida, utilize a função "write_file" para escrever o conteúdo desejado no arquivo. Por exemplo:

```Gleam
import File

File.write_file("meu_arquivo.txt", "Olá, mundo!")
```

Isso criará um arquivo chamado "meu_arquivo.txt" no diretório atual e escreverá a mensagem "Olá, mundo!" dentro dele. Você também pode utilizar variáveis e concatenar strings dentro da função "write_file" para criar arquivos mais dinâmicos.

## Mergulho Profundo

Além da função "write_file", o módulo "File" também possui outras funções úteis para gerenciamento de arquivos de texto, como "append_file" para adicionar conteúdo a um arquivo existente e "read_file" para ler o conteúdo de arquivos. É importante lembrar de sempre fechar o arquivo utilizando a função "close" após utilizá-lo.

Outra funcionalidade interessante do Gleam é a capacidade de lidar com caminhos de arquivos de forma segura e portátil, usando o módulo "Path" e suas funções, como "join" e "split". Isso garante que seus arquivos funcionarão corretamente em diferentes sistemas operacionais.

## Veja também

- Documentação oficial sobre arquivos e módulos de texto em Gleam: https://gleam.run/documentation/reference/03-modules.html#file
- Tutorial sobre escrita de arquivos em Gleam: https://101questions.net/guides/writing-files-in-gleam/
- Exemplos de código para manipulação de arquivos em Gleam: https://gist.github.com/topics/gleam-files