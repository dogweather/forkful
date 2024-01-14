---
title:                "Python: Utilizando expressões regulares"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por que

Se você for um programador Python, provavelmente já ouviu falar sobre expressões regulares. Elas são uma ferramenta extremamente útil para encontrar padrões em strings e fazer manipulações complexas. Ao aprender a usar expressões regulares, você pode economizar tempo e tornar seu código mais eficiente.

## Como fazer

Usar expressões regulares em Python é relativamente simples. Primeiro, importe o módulo "re":

```Python
import re
```

Em seguida, defina sua expressão regular usando a função "compile" do módulo "re":

```Python
expressao_regular = re.compile(r"padrao")
```

Você pode, então, usar esse objeto para realizar várias operações em strings, como encontrar correspondências, substituir trechos e muito mais. Veja um exemplo de código:

```Python
# Encontrar correspondências
cadeia_de_texto = "Este é um exemplo de uma cadeia de texto com uma correspondência."
correspondencias = expressao_regular.finditer(cadeia_de_texto)

# Iterar pelas correspondências encontradas
for correspondencia in correspondencias:
    print(correspondencia)
```

A saída desse código seria:

```Python
<re.Match object; span=(32, 42), match='correspondência'> 
```

## Exploração profunda

Embora a sintaxe básica das expressões regulares em Python seja simples, existem muitos recursos avançados que podem ser explorados para tornar suas expressões ainda mais poderosas. Alguns desses recursos incluem a utilização de metacaracteres, grupos, quantificadores e caracteres de escape.

Por exemplo, você pode usar o metacaractere "^" para encontrar correspondências no início de uma linha, ou "$" para encontrar corresponências no final de uma linha. Também é possível agrupar partes de uma expressão usando parênteses, permitindo que você manipule essas partes separadamente. Além disso, quantificadores como "+" e "*" podem ser usados para especificar a quantidade de ocorrências de um determinado padrão.

Uma das melhores maneiras de dominar as expressões regulares em Python é praticando. Experimente diferentes padrões e veja como eles se comportam em diferentes situações. A documentação oficial do módulo "re" também é uma ótima fonte de informação e exemplos.

## Veja também

- [Documentação oficial do módulo "re" do Python](https://docs.python.org/3/library/re.html)
- [Tutorial interativo de Expressões Regulares em Python](https://regexone.com/references/python)