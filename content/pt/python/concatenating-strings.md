---
title:                "Python: Concatenando strings"
simple_title:         "Concatenando strings"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que concatenar strings em Python?

A concatenação de strings é uma parte fundamental da programação em Python e é usada quando precisamos combinar duas ou mais strings em uma única string. Isso pode ser útil em várias situações, como na criação de mensagens personalizadas para usuários ou na formatação de saídas de dados.

## Como fazer?

Para concatenar strings em Python, podemos usar o operador "+" para unir as strings ou o método "join ()" para concatenar uma lista de strings.

```
# Exemplo 1: Usando o operador "+"
nome = "Maria"
sobrenome = "Silva"
nome_completo = nome + " " + sobrenome
print(nome_completo)
# Saída: Maria Silva

# Exemplo 2: Usando o método "join()"
lista_nomes = ["João", "Ana", "Pedro"]
nomes_completos = " e ".join(lista_nomes)
print(nomes_completos)
# Saída: João e Ana e Pedro
``` 

É importante lembrar que, ao usar o operador "+", todas as variáveis ​​devem ser do tipo string. Já com o método "join()", podemos usar uma lista com diferentes tipos de dados e ele irá converter tudo para string antes de concatenar.

Além disso, podemos usar o operador "+=" para adicionar uma string a uma variável existente.

```
# Exemplo 3:
frase = "Os gatos são "
frase += "animais incríveis!"
print(frase)
# Saída: Os gatos são animais incríveis!
```

## Aprofundando-se

Embora a concatenação de strings seja uma operação simples, é importante entender como ela funciona nos bastidores para evitar erros e otimizar o código.

Em Python, as strings são imutáveis, o que significa que elas não podem ser alteradas. Quando usamos o operador "+", na verdade estamos criando uma nova string com os valores das duas strings originais e atribuindo a uma nova variável. Isso pode ser um problema de desempenho, especialmente quando trabalhamos com grandes quantidades de dados.

Já com o método "join()", uma nova string é criada apenas no final, após todos os itens da lista serem concatenados. Isso torna o processo mais eficiente e recomendado para casos em que precisamos concatenar muitas strings.

## Veja também

- [Documentação oficial do Python sobre strings](https://docs.python.org/3/library/string.html)
- [Tutoriais sobre strings em Python (em português)](https://www.devmedia.com.br/concatenacao-de-strings-em-python/40537)