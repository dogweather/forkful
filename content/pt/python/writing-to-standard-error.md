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

## Por que

Escrever para o erro padrão (standard error) pode ser uma ferramenta útil para entender e depurar seu código em Python. Ele também pode ser usado para registrar informações importantes ou erros específicos durante a execução de um programa.

## Como fazer

Escrever para o erro padrão em Python é bastante simples. Basta usar o método `sys.stderr.write()` e passar uma string como parâmetro. Vamos dar uma olhada em um exemplo abaixo:

```python
import sys
sys.stderr.write("Este é um erro de exemplo.")
```

O código acima irá escrever "Este é um erro de exemplo." no erro padrão. No entanto, é importante observar que o texto não será imediatamente exibido no console. Ele será armazenado em um buffer até que o programa seja encerrado ou o buffer seja esvaziado. Portanto, é necessário esvaziar o buffer manualmente, usando o método `sys.stderr.flush()`:

```python
import sys
sys.stderr.write("Este é um erro de exemplo.")
sys.stderr.flush()
```

## Mergulho profundo

Apesar de simples, escrever para o erro padrão em Python pode ser uma ferramenta poderosa para identificar e resolver problemas de código. Além disso, ele pode ser usado em conjunto com o módulo `logging` para registrar informações relevantes durante a execução do programa.

É importante lembrar que o método `write` espera uma string como parâmetro. Portanto, se você quiser escrever outros tipos de dados, como números ou listas, é necessário converter para string primeiro.

## Veja também

- [Documentação oficial da biblioteca sys](https://docs.python.org/3/library/sys.html)
- [Tutorial de logging em Python](https://realpython.com/python-logging/)
- [Guia para depurar código em Python](https://realpython.com/python-debugging-pdb/)