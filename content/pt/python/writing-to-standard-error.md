---
title:                "Escrevendo para o erro padrão"
html_title:           "Python: Escrevendo para o erro padrão"
simple_title:         "Escrevendo para o erro padrão"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## O que e por que?

Escrever para o erro padrão (standard error) é uma técnica comum entre os programadores para registrar e comunicar informações importantes durante a execução de um código. Ele permite que os desenvolvedores acompanhem erros e avisos em tempo real, facilitando a identificação e correção de problemas em seus programas.

## Como fazer:

Um exemplo simples de como escrever para o erro padrão em Python seria:

```
import sys
print("Olá, mundo!", file=sys.stderr)
```

Este código imprime a mensagem "Olá, mundo!" no terminal padrão de erro. Observe que estamos usando o módulo `sys` e especificando o parâmetro `file=sys.stderr` ao imprimir a mensagem.

Aqui está o output esperado para este código:

```
$ python escrever_erro_padrao.py
Olá, mundo!
```

## Aprofundando:

Esta técnica é baseada em um conceito mais amplo chamado "redirecionamento de fluxo", que permite que o output de um programa seja direcionado para diferentes locais, como a tela ou um arquivo. O erro padrão é apenas um desses locais.

Alternativamente, os programadores também podem escrever para o erro padrão utilizando a função `logging.error()` do módulo `logging`. Isso permite um controle mais granular sobre o tipo e a formatação das mensagens de erro.

Na implementação do Python, o erro padrão é representado pelo objeto `sys.stderr`, que é definido automaticamente ao iniciar o interpretador.

## Veja também:

- [Documentação oficial do Python sobre o módulo `sys`](https://docs.python.org/3/library/sys.html)
- [Documentação oficial do Python sobre o módulo `logging`](https://docs.python.org/3/library/logging.html)
- [Tutorial sobre redirecionamento de fluxo em Python](https://realpython.com/python-logging/)