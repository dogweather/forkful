---
title:                "Escrevendo para o erro padrão"
html_title:           "C: Escrevendo para o erro padrão"
simple_title:         "Escrevendo para o erro padrão"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## O que e porque?

Escrever para o erro padrão (standard error) é simplesmente a prática de exibir mensagens de erro ou de depuração em vez de mensagens regulares. Isso é útil para os programadores porque permite que as mensagens de erro sejam separadas das mensagens normais e possam ser facilmente identificadas e corrigidas.

## Como fazer:

```
C printf("Mensagem de erro: Não foi possível abrir o arquivo.\n");
```

Saída:
```
Mensagem de erro: Não foi possível abrir o arquivo.
```

## Deep Dive:

No início do desenvolvimento de software, as mensagens de erro eram exibidas apenas no terminal ou linha de comando, tornando difícil a identificação de erros em códigos muito extensos. Com a introdução do conceito de "erro padrão", tornou-se possível separar as mensagens de erro das mensagens regulares e exibi-las em locais diferentes, facilitando a identificação e correção de erros.

Além disso, existem outras formas de lidar com mensagens de erro em C, como o uso da função ```perror()```, responsável por exibir uma mensagem de erro personalizada a partir do conteúdo da variável ```errno```. Isso permite uma maior personalização nas mensagens de erro e pode ser uma alternativa útil para exibir informações específicas em caso de problemas.

## See Also:

- [Tutorial sobre Mensagens de Erro em C](https://www.codingame.com/playgrounds/2205/manejo-de-erros-conceitos-basicos/tratamento-de-erros-no-c)
- [Documentação Oficial do C](https://devdocs.io/c/)