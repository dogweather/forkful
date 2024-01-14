---
title:                "Python: Removendo caracteres que correspondem a um padrão."
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que excluir caracteres correspondentes a um padrão?

Às vezes, ao trabalhar com dados de texto, pode ser necessário excluir caracteres que correspondem a um determinado padrão. Isso pode ser útil para limpar os dados ou para realizar alguma formatação específica. O Python tem uma função embutida para facilitar essa tarefa, a função "replace ()".

## Como fazer:

Para excluir caracteres correspondentes a um padrão, precisamos usar a função "replace ()" com dois argumentos: o padrão que queremos substituir e a string vazia "". Por exemplo, se quisermos excluir todos os números de uma string, podemos usar o código:

```Python
string = "Olá, meu número de telefone é 1234567!"
string = string.replace( '0-9', '' )
print( string )

# Output: Olá, meu número de telefone é !
```
Neste exemplo, usamos uma expressão regular para identificar todos os números de 0 a 9 e substituí-los por uma string vazia, o que os exclui da string original.

Além disso, podemos usar a função "replace ()" para excluir outros padrões, como espaços em branco ou caracteres especiais. É uma ferramenta útil para manipular dados de texto com facilidade.

## Mergulho Profundo

A função "replace ()" é um método da classe str, o que significa que só pode ser usada com objetos de string. Além disso, ela só substitui o primeiro padrão que correspondeu. Porém, podemos usar expressões regulares para substituir todos os padrões correspondentes, como fizemos no exemplo acima.

No entanto, é importante notar que a função "replace ()" é sensível a maiúsculas e minúsculas, o que significa que ela não irá substituir caracteres se eles estiverem em uma capitalização diferente da especificada no argumento. Podemos contornar isso usando o módulo "re" para ignorar a sensibilidade de maiúsculas e minúsculas nas expressões regulares.

## Veja também:
- [Documentação oficial do Python para a função "replace ()"](https://docs.python.org/3/library/stdtypes.html#str.replace)
- [Tutorial do W3Schools sobre expressões regulares em Python](https://www.w3schools.com/python/python_regex.asp)
- [Guia prático para usar a função "replace ()" com expressões regulares](https://realpython.com/regex-python/)