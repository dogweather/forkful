---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:54.267812-07:00
description: "Arrays associativos, ou tabelas hash, permitem armazenar dados como\
  \ pares de chave-valor, facilitando a organiza\xE7\xE3o e recupera\xE7\xE3o de informa\xE7\
  \xF5es por\u2026"
lastmod: '2024-03-13T22:44:46.996991-06:00'
model: gpt-4-0125-preview
summary: "Arrays associativos, ou tabelas hash, permitem armazenar dados como pares\
  \ de chave-valor, facilitando a organiza\xE7\xE3o e recupera\xE7\xE3o de informa\xE7\
  \xF5es por\u2026"
title: Usando arrays associativos
weight: 15
---

## O quê & Por quê?

Arrays associativos, ou tabelas hash, permitem armazenar dados como pares de chave-valor, facilitando a organização e recuperação de informações por chave. Eles são úteis quando você precisa de uma maneira mais estruturada para lidar com dados do que apenas listas, especialmente em configurações e ao lidar com uma gama de atributos.

## Como fazer:

Fish não suporta nativamente arrays associativos como o Bash 4+, mas você pode alcançar uma funcionalidade similar usando uma combinação de listas e manipulação de strings. Veja como imitá-los:

Primeiro, configurando elementos de "array associativo" separadamente:

```Fish Shell
set food_color_apple "red"
set food_color_banana "yellow"
```

Para acessar um elemento, basta referenciá-lo diretamente:

```Fish Shell
echo $food_color_apple
# Saída: red
```

Se você precisar iterar sobre eles, use um loop for considerando uma convenção de nomenclatura:

```Fish Shell
for food in apple banana
    echo $food_color_$food
end
# Saída:
# red
# yellow
```

Para aqueles que sentem falta do `${!array[@]}` do Bash para obter todas as chaves, você pode armazenar as chaves em uma lista separada:

```Fish Shell
set food_keys apple banana

for key in $food_keys
    echo $key 'é' $food_color_$key
end
# Saída:
# apple é red
# banana é yellow
```

## Aprofundamento

Verdadeiros arrays associativos, como em outras linguagens de script, ainda não fazem parte da abordagem do Fish. A solução alternativa mostrada aproveita as capacidades de manipulação de strings e listas do Fish para criar uma estrutura pseudo-associativa. Embora funcione, não é tão limpo ou à prova de erros quanto seria o suporte integrado de arrays associativos. Outros shells como Bash e Zsh fornecem funcionalidade de array associativo integrada, o que resulta em um código mais direto e legível. No entanto, a filosofia de design do Fish visa simplicidade e facilidade de uso, possivelmente às custas de tais recursos. A solução alternativa satisfaz a maioria das necessidades, mas fique de olho na evolução do Fish Shell - seus desenvolvedores melhoram ativamente e adicionam recursos baseados no feedback da comunidade.
