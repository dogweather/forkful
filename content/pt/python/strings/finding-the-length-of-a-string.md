---
title:                "Descobrindo o comprimento de uma string"
aliases:
- /pt/python/finding-the-length-of-a-string/
date:                  2024-01-20T17:48:08.401637-07:00
model:                 gpt-4-1106-preview
simple_title:         "Descobrindo o comprimento de uma string"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## O Que é & Porquê?
Descobrir o comprimento de uma string significa contar o número de caracteres que ela possui. Programadores fazem isso para validar entradas, delimitar processamentos de texto e para facilitar a manipulação de dados.

## Como Fazer:
Vamos ao código!

```python
# Exemplo simples para achar o comprimento de uma string
uma_string = "Olá, mundo!"
comprimento = len(uma_string)
print(comprimento)  # Saída: 12

# Contando caracteres em uma string com espaços
string_com_espacos = "Python é legal"
print(len(string_com_espacos))  # Saída: 14

# Strings vazias têm comprimento 0
string_vazia = ""
print(len(string_vazia))  # Saída: 0
```

## Aprofundando
A função `len()` é incorporada na linguagem Python e existe desde as primeiras versões. Ela é uma forma rápida e eficiente de contar quantos itens existem em diversos tipos de sequências, não apenas strings. Sob o capô, a função `len()` invoca o método especial `__len__` do objeto.

Alternativas? Em alguns casos, você poderia iterar sobre a string e contar os caracteres manualmente. Mas isso não é prático e vai contra a filosofia de Python de que "deve haver uma e preferencialmente apenas uma maneira óbvia de fazer algo". Além disso, usar `len()` é mais rápido e direto.

A implementação de `len()` é altamente otimizada para strings, já que elas são imutáveis e seu tamanho é guardado internamente pelo objeto da string. Portanto, `len()` simplesmente retorna este valor, sem precisar contar caractere por caractere.

## Veja Também
Para mais informações e práticas recomendadas em Python, confira os seguintes recursos:

- Python's built-in functions documentation: [Funções Incorporadas do Python](https://docs.python.org/pt-br/3/library/functions.html#len)
- Python Software Foundation: [Python.org](https://www.python.org/)
- A Python library for complex text processing: [TextBlob](https://textblob.readthedocs.io/en/dev/)

Estude outros métodos de strings e como Python lida com Unicode e codificação para aprimorar suas habilidades com processamento de texto.
