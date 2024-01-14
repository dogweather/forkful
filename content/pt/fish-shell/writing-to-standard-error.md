---
title:                "Fish Shell: Escrevendo no erro padrão"
simple_title:         "Escrevendo no erro padrão"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que

Muitas vezes, quando estamos escrevendo código em Fish Shell, queremos que nossos programas produzam resultados e informações de erro distintos. Ao escrever para o erro padrão, podemos controlar melhor o que aparece na tela do terminal e ajudar a identificar problemas com nosso código.

## Como Fazer

Um dos modos mais simples de escrever para o erro padrão em Fish Shell é utilizando o operador `>` junto com o número `2`. Por exemplo:

```
Fish Shell> echo "Este será mostrado na saída padrão"
Fish Shell> echo "Ooops, algo deu errado" >2
```
O primeiro comando irá imprimir "Este será mostrado na saída padrão" em nosso terminal, enquanto o segundo comando irá apenas escrever "Ooops, algo deu errado" na saída de erro padrão. Isso nos permite separar nossas mensagens de erro das mensagens de saída padrão, tornando mais fácil para nós identificar e corrigir problemas em nosso código.

## Mergulho Profundo

Em alguns casos, podemos querer redirecionar todas as nossas mensagens de erro para um arquivo em vez de mostrá-las no terminal. Para fazer isso, podemos usar o operador `&` junto com o operador `>` e especificar um arquivo para onde queremos redirecionar as mensagens de erro. Por exemplo:

```
Fish Shell> comando_que_pode_falhar > arquivo_saida 2>&1
```

Este comando irá redirecionar todas as mensagens de erro para o arquivo "arquivo_saida", enquanto ainda mostra as mensagens de saída padrão no terminal.

## Veja Também

- [Documentação sobre Redirecionamento em Fish Shell](https://fishshell.com/docs/current/tutorial.html#tutorial-redirection)
- [Tutorial sobre Redirecionamento em Fish Shell](https://www.mankier.com/1/tutorial-redirection)