---
changelog:
- 2024-04-04, dogweather, edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-20 17:43:02.363431-07:00
description: 'Como fazer: .'
lastmod: '2024-04-04T02:02:34.759874-06:00'
model: gpt-4-0125-preview
summary: .
title: "Excluindo caracteres que correspondem a um padr\xE3o"
weight: 5
---

## Como fazer:
```Python
import re

# Exemplo de string
texto = "Olá, Mundo! 1234"

# Remove todos os dígitos
sem_digitos = re.sub(r'\d', '', texto)
print(sem_digitos)  # Saída: "Olá, Mundo! "

# Remove pontuação
sem_pontuacao = re.sub(r'[^\w\s]', '', texto)
print(sem_pontuacao)  # Saída: "Olá Mundo 1234"

# Remove vogais
sem_vogais = re.sub(r'[aeiouAEIOU]', '', texto)
print(sem_vogais)  # Saída: "Ol, Mnd! 1234"
```

### Minha função personalizada

Faço isso com frequência suficiente para que eu refatorasse isso nesta simples função `delete()`. É também uma boa demonstração de [doctests](https://docs.python.org/3/library/doctest.html):

```python
def delete(string: str, regex: str) -> str:
    """
    >>> delete("Olá, mundo!", "l")
    'Oá, mundo!'

    >>> delete("Olá, mundo!", "[a-z]")
    'O, !'
    """
    return re.sub(regex, "", string)
```



## Aprofundamento
A prática de deletar caracteres que correspondem a um padrão em texto tem raízes profundas na ciência da computação, remontando a ferramentas Unix antigas como `sed` e `grep`. Em Python, o módulo `re` fornece essa capacidade, alavancando expressões regulares—uma ferramenta poderosa e versátil para o processamento de texto.

Alternativas ao módulo `re` incluem:
- Métodos de string como `replace()` para casos simples.
- Bibliotecas de terceiros como `regex` para padrões mais complexos e melhor suporte a Unicode.

Por baixo dos panos, quando você usa `re.sub()`, o interpretador Python compila o padrão em uma série de bytecodes, processados por uma máquina de estados que realiza a correspondência de padrões diretamente no texto de entrada. Esta operação pode ser intensiva em recursos para strings grandes ou padrões complexos, então considerações de desempenho são cruciais para o processamento de grandes volumes de dados.

## Veja também
- [Documentação do módulo Python `re`](https://docs.python.org/3/library/re.html): Documentação oficial para expressões regulares em Python.
- [Regular-Expressions.info](https://www.regular-expressions.info/): Um guia completo para expressões regulares.
- [Tutorial de Real Python sobre regex](https://realpython.com/regex-python/): Aplicações do mundo real de expressões regulares em Python.
