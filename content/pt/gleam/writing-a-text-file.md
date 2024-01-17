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

O que é e porquê escrever um arquivo de texto?

Escrever um arquivo de texto é o processo de criar e salvar informações em um formato legível por humanos. Programadores geralmente o fazem para armazenar dados estruturados, como configurações ou registros de eventos.

Como fazer:

Um arquivo de texto pode ser criado usando o comando ```Gleam.file.write()```, seguido pelo nome do arquivo e uma string de texto como conteúdo a ser salvo. Por exemplo:

```Gleam.file.write("meu_arquivo.txt", "Olá, mundo!")```

Isso criaria um arquivo chamado "meu_arquivo.txt" com o conteúdo "Olá, mundo!" dentro dele.

Mergulho profundo:

Escrever arquivos de texto é uma tarefa comum na programação, e é útil para armazenar e compartilhar informações. Antes dos computadores modernos, os arquivos de texto era