---
title:                "Python: Buscando e substituindo texto"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que
Às vezes, quando estamos trabalhando com texto em nossos programas Python, pode ser necessário fazer alterações em determinadas palavras ou frases. Usar a função de busca e substituição pode ser uma maneira fácil e eficaz de fazer isso.

## Como fazer
Para usar a função de busca e substituição em Python, você pode seguir os seguintes passos:

```
# Importe o módulo re (expressões regulares)
import re

# Crie uma string com o texto que você quer modificar
texto = "Hoje eu vou ao supermercado comprar leite e ovos."

# Use o método sub() do módulo re para fazer a substituição
novo_texto = re.sub("supermercado", "mercado", texto)

# Imprima o novo texto
print(novo_texto)
```

Saída:
```
Hoje eu vou ao mercado comprar leite e ovos.
```
Você também pode fazer várias substituições ao mesmo tempo, usando dicionários:

```
# Crie um dicionário com as palavras que você quer substituir
substituicoes = {
  "supermercado": "mercado",
  "leite": "pão",
  "ovos": "queijo"
}

# Utilize o mesmo código, mas agora passando o dicionário como parâmetro
novo_texto = re.sub("|".join(substituicoes.keys()), lambda match: substituicoes[match.group(0)], texto)

# Imprima o novo texto
print(novo_texto)
```

Saída:
```
Hoje eu vou ao mercado comprar pão e queijo.
```

## Aprofundando
Além de simplesmente substituir palavras, a função de busca e substituição em Python também pode ser utilizada com expressões regulares, permitindo que você faça modificações mais avançadas em seu texto. Por exemplo, você poderia substituir todas as ocorrências de números em seu texto por asteriscos.

```
# Utilize uma expressão regular para selecionar apenas números
novo_texto = re.sub("[0-9]+", "*", texto)
```

Saída:
```
Hoje eu vou ao supermercado comprar ** e *.
```

## Veja também
- [Documentação do módulo re em Python](https://docs.python.org/3/library/re.html)
- [Tutorial de Expressões Regulares em Python](https://www.datacamp.com/community/tutorials/python-regular-expression-tutorial)