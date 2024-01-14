---
title:    "Python: Busca e substituição de texto"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que utilizar a busca e substituição de texto em Python?

A busca e substituição de texto é uma ferramenta muito útil em programação, principalmente quando lidamos com grandes quantidades de texto. Isso pode ser feito facilmente em Python e pode economizar muito tempo e esforço durante o processo de desenvolvimento.

## Como fazer a busca e substituição de texto em Python?

Usando a função `replace()` em Python, podemos buscar e substituir um trecho de texto por outro. Veja o exemplo abaixo:

```Python
texto = "Olá, mundo!"
novo_texto = texto.replace("mundo", "Python")
print(novo_texto)
```

**Saída:**

```Olá, Python!```
Neste exemplo, substituímos a palavra "mundo" por "Python", resultando em "Olá, Python!". É importante notar que a função `replace()` é sensível a maiúsculas e minúsculas.

Podemos também utilizar a função `re.sub()` do módulo `re` para buscar e substituir textos usando expressões regulares. Abaixo, um exemplo de como substituir todas as vogais em uma frase por "x":

```Python
import re
texto = "Python é uma linguagem de programação incrível!"
novo_texto = re.sub("[aeiou]", "x", texto)
print(novo_texto)
```

**Saída:**

```Pxythxn x xmx lxnxgxgm dx pxgrxmxçxo xncrívxl!```

## Aprofundando na busca e substituição de texto

A função `replace()` é simples e eficaz para substituir textos específicos. No entanto, ao utilizar expressões regulares, podemos tornar a busca e substituição mais flexível e poderosa.

Algumas opções de substituição com expressões regulares em Python incluem:

- Substituir uma string por uma expressão regular e vice-versa
- Utilizar grupos de captura para acessar partes específicas do texto
- Utilizar a opção `count` para limitar o número de substituições feitas
- Utilizar a opção `flags` para modificar o comportamento da expressão regular

Para saber mais sobre expressões regulares em Python, confira a documentação oficial.

## Veja também

- [Documentação oficial do Python](https://www.python.org/)
- [Guia para expressões regulares em Python](https://docs.python.org/3/library/re.html)