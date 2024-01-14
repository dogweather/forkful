---
title:                "Python: Extraindo Substrings"
simple_title:         "Extraindo Substrings"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que extrair substrings em Python?

Extrair substrings, ou pedaços de texto específicos de uma string mais longa, é uma habilidade importante para programadores Python. Isso permite que você manipule e analise dados de forma mais eficiente, economizando tempo e esforço. Neste artigo, veremos como extrair substrings em Python.

## Como fazer

Para extrair substrings em Python, podemos usar o método `slice()` ou o operador de corte `:`. Vamos ver alguns exemplos:

```
# Criando uma string
frase = "Eu adoro programar em Python"

# Extraindo a primeira palavra
print(frase[:2]) # Saída: Eu

# Extraindo a segunda palavra
print(frase[4:11]) # Saída: programar

# Extraindo a última palavra
print(frase[-6:]) # Saída: Python
```

Podemos ver que o método `slice()` nos permite especificar o índice inicial e final para extrair a substring desejada. Já o operador de corte `:` nos permite extrair a substring a partir de um determinado ponto até o final da string. Também podemos usar índices negativos para extrair a substring a partir do final da string.

Outra forma de extrair substrings é usando o método `split()`, que divide uma string em uma lista de substrings com base em um determinado caractere ou sequência de caracteres. Vamos ver um exemplo:

```
# Criando uma string com várias palavras separadas por vírgulas
nomes = "Maria, João, Pedro, Ana"

# Extraindo a lista de nomes
print(nomes.split(", ")) # Saída: ['Maria', 'João', 'Pedro', 'Ana']
```

Podemos ver que, neste caso, definimos a vírgula como o caractere de separação. Isso nos permite extrair as palavras individuais da string original.

## Mergulho profundo

Além dos exemplos mencionados acima, existem várias outras maneiras de extrair substrings em Python. Por exemplo, podemos usar expressões regulares para encontrar e extrair padrões específicos de uma string. Também podemos usar o método `find()` para encontrar a posição de um determinado caractere ou sequência de caracteres em uma string e, em seguida, usar o método `slice()` para extrair a substring correspondente.

Outra técnica útil é usar o método `replace()` para substituir parte de uma string por outra substring. Isso pode ser útil quando queremos extrair uma parte específica de uma string e substituí-la por uma nova substring.

## Veja também

- [Documentação oficial sobre o método slice em Python](https://docs.python.org/3/library/functions.html#slice)
- [Tutorial sobre expressões regulares em Python](https://www.w3schools.com/python/python_regex.asp)
- [Tutorial sobre manipulação de strings em Python](https://www.learnpython.org/en/String_Manipulation)