---
date: 2024-01-20 17:35:33.067603-07:00
description: "Concatenar strings significa juntar texto de duas ou mais strings, formando\
  \ uma \xFAnica sequ\xEAncia. Programadores fazem isso para construir mensagens,\u2026"
lastmod: '2024-03-11T00:14:19.811656-06:00'
model: gpt-4-1106-preview
summary: "Concatenar strings significa juntar texto de duas ou mais strings, formando\
  \ uma \xFAnica sequ\xEAncia. Programadores fazem isso para construir mensagens,\u2026"
title: Concatenando strings
---

{{< edit_this_page >}}

## O Que & Porquê?
Concatenar strings significa juntar texto de duas ou mais strings, formando uma única sequência. Programadores fazem isso para construir mensagens, combinar dados de texto, ou preparar informações para serem exibidas para usuários.

## Como Fazer:
```Python
# Concatenação simples com +
nome = "Mundo"
boas_vindas = "Olá, " + nome + "!"
print(boas_vindas)  # Output: Olá, Mundo!

# Usando .join() para concatenar uma lista de strings
lista_de_palavras = ["Python", "é", "super", "legal"]
frase = " ".join(lista_de_palavras)
print(frase)  # Output: Python é super legal

# Interpolação de string com f-strings
linguagem = "Python"
versao = "3.10"
mensagem = f"Você está rodando o {linguagem} na versão {versao}."
print(mensagem)  # Output: Você está rodando o Python na versão 3.10.
```

## Mergulho Profundo
Concatenação de strings é um conceito tão antigo quanto as próprias linguagens de programação. Em Python, além do operador `+` e do método `.join()`, que são bem diretos, temos também a interpolação de strings com as chamadas f-strings (introduzidas no Python 3.6), que são uma maneira mais eficiente e legível de combinar strings e variáveis. Antigamente, utilizava-se o operador `%` e o método `.format()`, mas com as f-strings isso se tornou menos comum por serem menos verbosos e mais rápidos.

## Veja Também
Aqui estão alguns links que podem ajudar a expandir seu conhecimento:
- [Documentação oficial do Python sobre f-strings](https://docs.python.org/3/reference/lexical_analysis.html#f-strings)
- [Artigo sobre a eficiência das f-strings](https://realpython.com/python-f-strings/)
- [Documentação para o método `.join()`](https://docs.python.org/3/library/stdtypes.html#str.join)
- [Guia sobre o operador `%` e o método `.format()` para formatação de strings](https://pyformat.info/)
