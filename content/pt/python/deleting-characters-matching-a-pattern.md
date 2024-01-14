---
title:    "Python: Excluindo caracteres que correspondem a um padrão"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que deletar caracteres que correspondem a um padrão?

Imagine que você está trabalhando em um projeto que envolve a manipulação de strings em Python. Em determinado momento, você percebe que precisa remover todos os caracteres que correspondem a um determinado padrão. Isso pode ser bastante útil em diversas situações, como limpar dados de entrada ou formatar strings de acordo com uma determinada máscara. Para fazer isso de forma eficiente, é importante saber como deletar caracteres que seguem um determinado padrão em Python.

## Como fazer?

Você pode usar o método `re.sub()` do módulo `re` para substituir caracteres que correspondem a uma expressão regular por uma string vazia. Veja um exemplo de código:

```
import re

# String original
string = "Oi, tudo bem?# Como vai você?"

# Removendo caracteres que são dígitos 
nova_string = re.sub(r'\d+', '', string)

# Imprimindo resultado
print(nova_string)

# Saída: Oi, tudo bem?# Como vai você?
```

Neste exemplo, usamos a expressão regular `\d+` para encontrar todos os dígitos na string e substituí-los por uma string vazia. É importante notar que o método `sub()` retorna uma nova string e não altera a string original.

Você também pode usar a função `filter()` junto com a função `lambda` para filtrar os caracteres indesejados. Veja outro exemplo:

```
# Removendo caracteres maiúsculos da string
nova_string = "".join(filter(lambda x: x.islower(), string))

# Imprimindo resultado
print(nova_string)

# Saída: oi, tudo bem?# como vai você?
```
Neste caso, usamos a função `filter()` para filtrar apenas os caracteres minúsculos na string e depois usamos a função `join()` para juntá-los novamente em uma nova string.

Existem também outras formas de realizar essa tarefa, como por exemplo o uso da função `translate()` ou a criação de uma função personalizada. O importante é entender os conceitos por trás dessas soluções para que você possa aplicá-las de acordo com suas necessidades.

## Mergulho mais profundo

Ao lidar com strings em Python, é importante conhecer as expressões regulares e como utilizá-las para manipular dados de forma mais eficiente. Além disso, é fundamental entender os métodos e funções disponíveis no módulo `re` e como aplicá-los em diferentes problemas. Investir um tempo para aprender esses conceitos pode poupar muito trabalho manual e garantir que suas strings estejam sempre no formato necessário.

## Veja também

- [Documentação oficial do módulo `re`](https://docs.python.org/3/library/re.html)
- [Tutorial de expressões regulares em Python](https://docs.python.org/3/howto/regex.html)
- [Funcionamento do método `sub()`](https://www.regular-expressions.info/python.html#sub)