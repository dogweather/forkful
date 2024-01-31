---
title:                "Usando arrays associativos"
date:                  2024-01-30T19:09:58.147811-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando arrays associativos"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Que?

Arrays associativos são como arrays superpotentes que permitem o uso de strings como índices, ao invés de apenas inteiros. Programadores os utilizam para estruturas de dados mais complexas, facilitando o manejo de dados que não se encaixam de forma ordenada em uma lista sequencial.

## Como Fazer:

Primeiro, declare um array associativo no Bash:

```Bash
declare -A my_array
```

Depois, você pode começar a preenchê-lo com valores, usando strings como chaves:

```Bash
my_array["nome"]="Linux Journal"
my_array["tema"]="Programação"
```

Para acessar um elemento, use sua chave:

```Bash
echo ${my_array["nome"]}  # Saída: Linux Journal
```

Iterar sobre chaves e valores também é simples:

```Bash
for chave in "${!my_array[@]}"; do
    echo "$chave: ${my_array[$chave]}"
done
```

A saída de exemplo poderia ser assim:

```
nome: Linux Journal
tema: Programação
```

Para adicionar ou modificar elementos, apenas atribua um valor a uma chave, de maneira similar à população inicial:

```Bash
my_array["leitores"]="Você"
```

E para remover um elemento, use `unset`:

```Bash
unset my_array["tema"]
```

## Aprofundamento

Arrays associativos foram introduzidos na versão 4.0 do Bash, tornando-os uma adição relativamente recente à linguagem. Antes de sua introdução, lidar com arrays de índices não inteiros era complicado, muitas vezes requerendo soluções alternativas ou ferramentas externas como `awk` ou `sed`.

Por trás dos panos, o Bash implementa arrays associativos usando tabelas de hash. Essa implementação permite uma busca por chave eficiente, que permanece bastante constante independente do tamanho do array, uma característica crítica para o desempenho na execução de scripts.

Embora os arrays associativos no Bash tragam muita potência e flexibilidade para a programação em shell, eles vêm com seu próprio conjunto de limitações, tais como serem um tanto quanto mais complicados para trabalhar em comparação com arrays em linguagens de nível mais alto como Python ou JavaScript. Para tarefas de manipulação de dados complexas, ainda pode valer a pena considerar ferramentas ou linguagens externas mais adequadas para o trabalho.

No entanto, para muitas tarefas típicas de script, os arrays associativos fornecem uma ferramenta valiosa no kit de ferramentas do programador Bash, permitindo scripts mais legíveis e sustentáveis ao permitir o uso de chaves de string significativas ao invés de índices numéricos.
