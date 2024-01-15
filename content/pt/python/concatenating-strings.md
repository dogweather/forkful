---
title:                "Concatenando strings."
html_title:           "Python: Concatenando strings."
simple_title:         "Concatenando strings."
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que

Você já se perguntou como os desenvolvedores conseguem unir palavras e frases em seus códigos? Essa é uma técnica muito importante e útil chamada de concatenar strings. Neste artigo, você aprenderá por que é importante entender essa função e como implementá-la em seus projetos em Python.

## Como Fazer

A concatenação de strings é o processo de combinar duas ou mais strings em uma única string. Em Python, podemos fazer isso utilizando o operador "+". Vamos dar uma olhada no exemplo abaixo:

```Python
nome = "João"
sobrenome = "Silva"
nome_completo = nome + sobrenome
print(nome_completo)
```
**Saída:** JoãoSilva

Neste exemplo, criamos duas variáveis chamadas "nome" e "sobrenome" e as combinamos usando o operador "+". É importante lembrar que não há espaço entre as strings, por isso é necessário adicionar manualmente caso deseje que seja exibido na saída.

Você também pode concatenar strings adicionando uma string a si mesma, como mostrado no exemplo abaixo:

```Python
frase = "Python "
nova_frase = frase * 3
print(nova_frase)
```
**Saída:** Python Python Python 

O código acima irá imprimir a string "Python" três vezes seguidas. É importante notar que o operador "*" só pode ser utilizado para multiplicar uma string por um inteiro.

Outra forma de concatenar strings em Python é utilizando o método "format". Este método permite combinar strings com outros tipos de dados, como números inteiros e floats. Veja o exemplo abaixo:

```Python
idade = 25
frase = "Eu tenho {} anos de idade."
mensagem = frase.format(idade)
print(mensagem)
```
**Saída:** Eu tenho 25 anos de idade.

No código acima, utilizamos o par de chaves "{}" no lugar do valor da idade e, em seguida, atribuímos o valor da variável "idade" ao método "format". Isso nos permite criar strings dinâmicas e mais complexas.

## Detalhando mais sobre a concatenação de strings

Além do operador "+" e do método "format", Python também possui outras maneiras de concatenar strings, como o método "join" e o método "f-strings". O método "join" permite unir strings a partir de uma lista ou tupla de strings, enquanto o método "f-strings" nos permite inserir valores de variáveis diretamente em uma string.

Outra coisa importante a lembrar é que, em Python, as strings são imutáveis, o que significa que não podemos alterá-las diretamente. Portanto, toda vez que concatenamos duas strings, uma nova string é criada e a original permanece inalterada.

Em casos em que precisamos concatenar um grande número de strings, é recomendado utilizar o método "join", pois é mais eficiente em termos de desempenho do que usar o operador "+".

## Veja também

Confira mais informações sobre como trabalhar com strings em Python nos links abaixo:

- [Documentação oficial do Python sobre strings](https://docs.python.org/3/library/string.html)
- [Tutorial de concatenação de strings em Python](https://www.geeksforgeeks.org/python-string-concatenation/)
- [Vídeo-aula sobre a concatenação de strings em Python](https://www.youtube.com/watch?v=fMH6IjVO6Fg)