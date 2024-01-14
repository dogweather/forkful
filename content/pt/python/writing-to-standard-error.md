---
title:                "Python: Escrevendo no erro padrão"
simple_title:         "Escrevendo no erro padrão"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que escrever para a saída de erro em Python?

Escrever para a saída de erro em programação é uma prática importante para a depuração de código e identificação de possíveis erros e problemas. Ao imprimir mensagens de erro específicas, podemos entender melhor o que está acontecendo no nosso código e como corrigir esses problemas.

## Como fazê-lo?

Podemos escrever para a saída de erro em Python usando a função `print()` com a opção `file=sys.stderr`. Isso garantirá que a mensagem de erro seja impressa na saída de erro padrão, que é separada da saída de impressão padrão.

Por exemplo, se tivermos um código que divide dois números, mas o segundo número é 0, isso resultaria em um erro de divisão por zero. Podemos imprimir uma mensagem de erro usando a saída de erro padrão da seguinte forma:

```Python
num1 = 10
num2 = 0
resultado = num1 / num2
print("Resultado da divisão: ", resultado, file=sys.stderr)
```

Isso imprimirá a mensagem de erro "ZeroDivisionError: division by zero" na saída de erro padrão, alertando-nos sobre o erro e facilitando a depuração.

## Mergulho profundo

Além de imprimir mensagens de erro simples, também podemos usar a função `print()` para escrever informações mais detalhadas na saída de erro. Por exemplo, podemos adicionar informações extras, como o nome da variável que causou o erro ou uma mensagem mais descritiva sobre o que estava sendo feito no momento do erro. Isso pode nos ajudar a compreender mais facilmente o que podemos fazer para corrigir o problema.

Outra prática útil é usar a instrução `try/except` para capturar exceções e imprimir mensagens de erro personalizadas. Dessa forma, podemos tratar os erros e imprimir informações mais específicas ao mesmo tempo.

## Veja também

- [Documentação oficial do Python sobre a função print()](https://docs.python.org/3/library/functions.html#print)
- [Tutorial sobre tratamento de exceções em Python](https://www.w3schools.com/python/python_try_except.asp)