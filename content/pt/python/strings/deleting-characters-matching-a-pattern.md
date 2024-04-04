---
changelog:
- 2024-04-04, dogweather, edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-20 17:43:02.363431-07:00
description: 'Como fazer: .'
lastmod: '2024-04-04T01:27:42.959137-06:00'
model: gpt-4-0125-preview
summary: .
title: "Excluindo caracteres que correspondem a um padr\xE3o"
weight: 5
---

## Como fazer:

```Python
import re

# Exemplo de string
text = "Hello, World! 1234"

# Remover todos os dígitos
sem_digitos = re.sub(r'\d', '', text)
print(sem_digitos)  # Saída: "Hello, World! "

# Remover pontuação
sem_pontuacao = re.sub(r'[^\w\s]', '', text)
print(sem_pontuacao)  # Saída: "Hello World 1234"

# Remover vogais
sem_vogais = re.sub(r'[aeiouAEIOU]', '', text)
print(sem_vogais)  # Saída: "Hll, Wrld! 1234"
```

### Uma função personalizada que escrevi

Faço isso com frequência suficiente que refatorei isso na função `delete()`. É também uma boa demonstração de [doctests](https://docs.python.org/3/library/doctest.html):

```python
def delete(string: str, regex: str) -> str:
    """
    >>> delete("Hello, world!", "l")
    'Heo, word!'

    >>> delete("Hello, world!", "[a-z]")
    'H, !'
    """
    return re.sub(regex, "", string)
```



## Mergulho Profundo
A prática de deletar caracteres que correspondem a um padrão em texto tem raízes profundas na ciência da computação, remontando a ferramentas Unix antigas como `sed` e `grep`. No Python, o módulo `re` fornece essa capacidade, aproveitando expressões regulares — uma ferramenta poderosa e versátil para processamento de texto.

Alternativas para o módulo `re` incluem:
- Métodos de string como `replace()` para casos simples.
- Bibliotecas de terceiros como `regex` para padrões mais complexos e melhor suporte a Unicode.

Por debaixo dos panos, quando você usa `re.sub()`, o interpretador Python compila o padrão em uma série de bytecodes, processados por uma máquina de estado que realiza a correspondência de padrões diretamente no texto de entrada. Esta operação pode ser intensiva em recursos para strings grandes ou padrões complexos, portanto, considerações de desempenho são cruciais para o processamento de grandes volumes de dados.

## Veja Também
- [Documentação do módulo `re` do Python](https://docs.python.org/3/library/re.html): Documentos oficiais para expressões regulares no Python.
- [Regular-Expressions.info](https://www.regular-expressions.info/): Um guia abrangente sobre expressões regulares.
- [Tutorial do Real Python sobre regex](https://realpython.com/regex-python/): Aplicações do mundo real de expressões regulares em Python.
