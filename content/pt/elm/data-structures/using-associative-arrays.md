---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:53.839837-07:00
description: "Arrays associativos, ou como o Elm os chama, Dicion\xE1rios, mapeiam\
  \ chaves para valores de uma forma que torna a busca, inser\xE7\xE3o e exclus\xE3\
  o de valores\u2026"
lastmod: '2024-03-11T00:14:20.193904-06:00'
model: gpt-4-0125-preview
summary: "Arrays associativos, ou como o Elm os chama, Dicion\xE1rios, mapeiam chaves\
  \ para valores de uma forma que torna a busca, inser\xE7\xE3o e exclus\xE3o de valores\u2026"
title: Usando arrays associativos
---

{{< edit_this_page >}}

## O Que & Porquê?

Arrays associativos, ou como o Elm os chama, Dicionários, mapeiam chaves para valores de uma forma que torna a busca, inserção e exclusão de valores extremamente rápidas. Eles são sua melhor opção quando você precisa acompanhar coisas sem uma ordem estrita, como preferências de usuários ou listas de inventário.

## Como:

No Elm, você trabalha com Dicionários no módulo `Dict`, então vamos mergulhar em um exemplo rápido:

```Elm
import Dict exposing (Dict)

-- Inicializando um dicionário com chaves String e valores Int
exampleDict : Dict String Int
exampleDict = Dict.fromList [("apple", 5), ("banana", 2), ("orange", 8)]

-- Adicionando ou atualizando um valor
updatedDict = Dict.insert "grape" 10 exampleDict

-- Recuperando um valor (note o tipo Maybe, já que a chave pode não estar presente)
fruitCount : Maybe Int
fruitCount = Dict.get "apple" updatedDict

-- Removendo um par chave-valor
finalDict = Dict.remove "banana" updatedDict

-- Convertendo um dicionário de volta para uma lista
dictToList = Dict.toList finalDict
```

Saída de amostra ao exibir `dictToList`:

```Elm
[("apple", 5), ("grape", 10), ("orange", 8)]
```

Isso demonstra as operações básicas: criar, atualizar, acessar e iterar sobre um Dicionário.

## Aprofundamento

Os Dicionários no Elm internamente usam uma estrutura conhecida como árvore AVL - um tipo de árvore binária de busca autoequilibrada. Essa escolha equilibra garantir que operações como inserir, obter e remover tenham bom desempenho (complexidade de tempo logarítmico) e manter a simplicidade no manuseio dos dados.

Apesar das forças do `Dict` do Elm, ele não é uma solução que serve para todos os casos. Para coleções que são ordenadas ou precisam ser iteradas sequencialmente, Lista ou Array podem ser mais apropriados. Além disso, ao trabalhar com um conjunto fixo de chaves conhecidas, usar tipos personalizados (a versão do Elm de enums) pode oferecer mais segurança de tipo e uma intenção mais clara no seu código.

No ecossistema do Elm, `Dict` oferece uma maneira confiável de gerenciar coleções de pares chave-valor onde as chaves são únicas e a ordem não importa. Embora estruturas mais novas ou mais sofisticadas possam surgir, o módulo `Dict` permanece como uma ferramenta fundamental no kit de ferramentas do programador Elm por sua simplicidade e eficiência em lidar com arrays associativos.
