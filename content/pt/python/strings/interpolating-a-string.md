---
changelog:
- 2024-01-28, dogweather, reviewed
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:24:01.169975-07:00
description: "A interpola\xE7\xE3o de strings \xE9 o m\xE9todo de embutir express\xF5\
  es dentro de literais de string. Programadores a utilizam para inserir valores dinamicamente\
  \ em\u2026"
lastmod: '2024-03-13T22:44:46.140016-06:00'
model: gpt-4-0125-preview
summary: "A interpola\xE7\xE3o de strings \xE9 o m\xE9todo de embutir express\xF5\
  es dentro de literais de string. Programadores a utilizam para inserir valores dinamicamente\
  \ em\u2026"
title: Interpolando uma string
weight: 8
---

## O Que & Por Que?
A interpolação de strings é o método de embutir expressões dentro de literais de string. Programadores a utilizam para inserir valores dinamicamente em strings, o que torna o código mais legível e limpo do que a tradicional concatenação de strings.

## Como fazer:
No Python 3.6 e versões posteriores, você pode interpolar strings usando f-strings. Veja como:

```Python
name = 'Alice'
age = 30
greeting = f"Olá, {name}. Você tem {age} anos."

print(greeting)
```

Saída:
```
Olá, Alice. Você tem 30 anos.
```

Você também pode usar expressões dentro das chaves:

```Python
a = 5
b = 10
info = f"Cinco mais dez é {a + b}, não {2 * (a + b)}."

print(info)
```

Saída:
```
Cinco mais dez é 15, não 30.
```

## Mergulho Profundo
Antes do Python 3.6, `.format()` era o método usado para interpolação de strings:

```Python
name = 'Bob'
age = 25
greeting = "Olá, {}. Você tem {} anos.".format(name, age)

print(greeting)
```

O Python antigo (versões < 2.6) usava o operador `%` para interpolação, o que é menos intuitivo e pode se tornar confuso com múltiplas variáveis:

```Python
name = 'Carol'
age = 35
greeting = "Olá, %s. Você tem %d anos." % (name, age)

print(greeting)
```

Além de uma sintaxe mais limpa, as f-strings são mais rápidas porque são avaliadas em tempo de execução e então convertidas diretamente em uma operação eficiente de formatação de string. O método `.format()` e o operador `%` envolvem mais etapas e são mais lentos.

## Veja Também
- [PEP 498 – Interpolação Literal de Strings](https://www.python.org/dev/peps/pep-0498/) para a documentação oficial sobre f-strings.
- [Python f-strings](https://realpython.com/python-f-strings/) por Real Python para um tutorial sobre como usar f-strings.
- [O Método .format()](https://docs.python.org/3/library/stdtypes.html#str.format) na documentação do Python para entender o método mais antigo `.format()` de formatação de strings.
