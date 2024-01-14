---
title:    "Python: Capitalizando uma string."
keywords: ["Python"]
---

{{< edit_this_page >}}

## Por que capitalizar uma string em Python?

Capitalize é uma função bastante útil em programação, especialmente quando se trabalha com strings em Python. Ela nos permite modificar uma string, tornando a primeira letra de cada palavra maiúscula. Isso pode ser útil em várias situações, como por exemplo, ao formatar nomes de pessoas ou títulos de livros.

## Como usar a função capitalize em Python

Para utilizar a função capitalize, primeiro precisamos definir uma string.

```
nome = "maria santos"
```

Agora, para capitalizar essa string, basta usar a função capitalize da seguinte forma:

```
nome.capitalize()
```

O output dessa instrução será: "Maria Santos".

Podemos também usar a função capitalize em uma lista de strings, como no exemplo abaixo:

```
nomes = ["joão silva", "maria santos", "pedro alves"]

for nome in nomes:
  print(nome.capitalize())
```

O output dessa instrução será:

```
João Silva
Maria Santos
Pedro Alves
```

## Aprofundando na função capitalize

A função capitalize não se limita apenas a tornar a primeira letra de cada palavra maiúscula. Ela também respeita as regras de capitalização da língua que está sendo utilizada.

Por exemplo, em inglês, a função capitalize irá transformar a primeira letra de cada palavra em maiúscula, exceto por preposições e artigos.

Já em português, a função irá sempre tornar maiúscula apenas a primeira letra da string.

```
frase = "o menino foi ao parque"
frase.capitalize()
```
O output dessa instrução será: "O menino foi ao parque".

Porém, é importante notar que a função capitalize não altera as letras maiúsculas já existentes na string.

```
nome = "João Silva"
nome.capitalize()
```
O output dessa instrução será: "João silva". Isso acontece porque a função apenas altera a primeira letra de cada palavra, mantendo as outras letras como estavam.

## Veja também

- [Python String capitalize() Method](https://www.w3schools.com/python/ref_string_capitalize.asp)
- [Capitalizing strings in Python](https://www.geeksforgeeks.org/capitalizing-first-letter-of-each-word-in-python/)
- [String capitalization in different languages](https://stackoverflow.com/questions/2365411/string-capitalization-in-different-languages)