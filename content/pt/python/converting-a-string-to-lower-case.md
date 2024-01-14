---
title:    "Python: Convertendo uma string para letras minúsculas"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Por que

Converter strings para letras minúsculas é importante para garantir uniformidade e consistência no processamento de dados, independentemente de como a string foi inserida ou formatada originalmente. Isso torna mais fácil realizar operações de comparação e busca em strings.

## Como fazer

Para converter uma string para letras minúsculas em Python, podemos utilizar o método `lower()`. Este método retorna uma cópia da string original com todas as letras convertidas para minúsculas. Por exemplo:

```Python
string = "Olá Mundo!"
string_lower = string.lower()

print(string_lower)
```
**Saída: olá mundo!**

Podemos também utilizar o método `casefold()`, que é uma versão mais agressiva de converter letras para minúsculas. Este método é útil quando precisamos comparar strings sem levar em conta acentos e outros caracteres especiais. Por exemplo:

```Python
string = "Olá Mundo!"
string_casefold = string.casefold()

print(string_casefold)
```
**Saída: ola mundo!**

Note que o método `casefold()` também converte a letra "l" com acento para "l" sem acento.

## Profundando Mais

Além dos métodos `lower()` e `casefold()`, existem maneiras de converter strings para minúsculas utilizando funções e expressões regulares em Python. Também é importante mencionar que a conversão para minúsculas pode ser aplicada apenas a letras alfabéticas, mantendo números e outros caracteres intactos. É importante entender os diferentes métodos e técnicas disponíveis e escolher a mais adequada para a sua aplicação.

## Veja também

- [Documentação oficial do Python para o método `lower()`](https://docs.python.org/3/library/stdtypes.html#str.lower)
- [Documentação oficial do Python para o método `casefold()`](https://docs.python.org/3/library/stdtypes.html#str.casefold)
- [Tutorial sobre expressões regulares em Python](https://www.digitalocean.com/community/tutorials/how-to-use-regular-expressions-in-python-3)