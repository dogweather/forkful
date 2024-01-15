---
title:                "Excluindo caracteres que correspondem a um padrão"
html_title:           "Python: Excluindo caracteres que correspondem a um padrão"
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que

Você provavelmente já se deparou com a situação de precisar remover alguns caracteres específicos de uma string. Isso pode ser útil, por exemplo, para limpar dados de um arquivo ou para validar entradas de usuário. Neste artigo, aprenderemos como deletar caracteres que correspondem a um determinado padrão em Python.

## Como fazer

Para deletar caracteres que correspondem a um padrão em uma string, podemos utilizar o método `replace()` combinado com o método `filter()` da classe `str`. Veja um exemplo abaixo:

```Python
# Definindo uma string
texto = "ABCDE12345"

# Utilizando replace() e filter()
novo_texto = "".join(filter(lambda x: not x.isdigit(), texto.replace("A", "")))

# Imprimindo o resultado
print(novo_texto)

# Resultado: BCDE
```

Neste exemplo, utilizamos o `replace()` para remover todas as ocorrências da letra "A" na string e, em seguida, combinamos com o `filter()` para remover todos os dígitos restantes. Por fim, utilizamos o `"".join()` para transformar os caracteres resultantes em uma nova string.

Outra maneira de fazer isso é utilizando expressões regulares. Veja um exemplo:

```Python
import re

# Definindo uma string
texto = "ABCDE12345"

# Utilizando regex
novo_texto = re.sub("[A0-9]", "", texto)

# Imprimindo o resultado
print(novo_texto)

# Resultado: BCDE
```

Neste exemplo, utilizamos a função `sub()` do módulo `re` para substituir todas as ocorrências de letras e dígitos pela string vazia, resultando em uma string limpa apenas com as letras desejadas.

## Mergulho profundo

Ambos os métodos apresentados acima são eficientes para deletar caracteres que correspondem a um padrão em uma string. No entanto, é importante lembrar que o método `replace()` é sensível a maiúsculas e minúsculas, enquanto o uso de expressões regulares pode ser mais complexo e demandar um maior conhecimento sobre o assunto.

No caso do `filter()`, é importante destacar que ele retorna um objeto do tipo `filter`, não uma string. Por isso, é necessário utilizar o método `join()` para transformar os resultados em uma nova string.

## Veja também

- [Documentação oficial do Python](https://docs.python.org/3/howto/regex.html)
- [Tutorial sobre expressões regulares em Python](https://realpython.com/regex-python/)