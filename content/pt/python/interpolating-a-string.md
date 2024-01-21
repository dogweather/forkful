---
title:                "Interpolando uma string"
date:                  2024-01-20T17:51:25.604123-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolando uma string"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/interpolating-a-string.md"
---

{{< edit_this_page >}}

## O Quê & Porquê?
Interpolar strings é o processo de injetar variáveis ou expressões diretamente numa cadeia de texto. Programadores usam isso para montar mensagens dinâmicas, economizando tempo e código.

## Como Fazer:
```Python
# Interpolação usando f-strings (disponível a partir do Python 3.6)
nome = "Joana"
mensagem = f"Olá, {nome}! Como vai você?"
print(mensagem)  # Saída: Olá, Joana! Como vai você?

# Uso de format() em versões anteriores do Python
idade = 30
mensagem = "Você tem {} anos.".format(idade)
print(mensagem)  # Saída: Você tem 30 anos.

# Outra opção, porém menos usada: operador % 
altura = 1.75
mensagem = "Sua altura é %.2f metros." % altura
print(mensagem)  # Saída: Sua altura é 1.75 metros.
```

## Mergulho Profundo
As f-strings foram introduzidas no Python 3.6 e são, hoje, o método preferido para interpolar strings devido à sua legibilidade e eficiência. Antes disso, utilizava-se o método `format()` ou o operador `%`, também conhecidos como "string formatting". 

O método `format()` é bastante flexível e ainda é usado, permitindo configurações complexas como formatação de números e alinhamento. Já o operador `%`, inspirado na linguagem C, é considerado obsoleto e menos legível, mas ainda é encontrado em códigos antigos.

A interpolação de strings ajuda a organizar o código, pois evita a concatenação manual e torna o texto mais conciso. Quando as f-strings surgiram, tornaram esse processo significativamente mais rápido, pois são implementadas em tempo de compilação, e não em tempo de execução como as outras opções.

## Veja Também
- Documentação Oficial do Python sobre Formatação de Strings: https://docs.python.org/3/library/string.html#formatstrings
- PEP 498, que introduziu f-strings: https://www.python.org/dev/peps/pep-0498/
- Tutorial compreensivo sobre f-strings: https://realpython.com/python-f-strings/