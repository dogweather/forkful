---
title:                "Criando um arquivo temporário"
html_title:           "Gleam: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que

Criar um arquivo temporário pode ser necessário em diversas situações ao programar em Gleam. Isso pode facilitar o armazenamento de dados temporários ou a manipulação de arquivos sensíveis.

## Como fazer

Criar um arquivo temporário em Gleam é bastante simples. Veja o exemplo abaixo:

```Gleam
let arquivo_temporario = std.file.create_temporary_file("meu_arquivo.txt")
```

O código acima irá criar um arquivo temporário chamado "meu_arquivo.txt" e atribuí-lo à variável "arquivo_temporario". Para escrever conteúdo no arquivo, podemos utilizar a função "write" da biblioteca padrão de arquivos.

```Gleam
std.file.write(arquivo_temporario, "Olá, mundo!")
```

Podemos então verificar o conteúdo do arquivo lendo-o com a função "read" e imprimindo o resultado no console:

```Gleam
let conteudo = std.file.read(arquivo_temporario)
let _ = std.io.print_line(conteudo)
```

A saída do programa será "Olá, mundo!". Depois de utilizarmos o arquivo temporário, é importante que o excluamos para liberar espaço em disco. Isso pode ser feito com a função "remove" da biblioteca de arquivos.

```Gleam
std.file.remove(arquivo_temporario)
```

E pronto! Agora temos um arquivo temporário criado, escrito e lido com sucesso.

## Mais detalhes

Quando criamos um arquivo temporário em Gleam, ele será armazenado em um diretório padrão do sistema. Isso pode variar de acordo com o sistema operacional, mas geralmente é na pasta "Temp". Além disso, é importante lembrar de sempre excluir o arquivo após seu uso, pois ele não será excluído automaticamente.

## Veja também

- [Documentação da biblioteca padrão de arquivos em Gleam](https://gleam.run/modules/stdlib/file.html)
- [Tutorial de Gleam no site oficial](https://gleam.run/getting-started/introduction.html)