---
title:                "Escrevendo testes"
html_title:           "Fish Shell: Escrevendo testes"
simple_title:         "Escrevendo testes"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/writing-tests.md"
---

{{< edit_this_page >}}

## O que e Por que?

Escrever testes como parte do processo de desenvolvimento de software é uma prática comum entre programadores. Esses testes são códigos escritos para verificar se o código principal está funcionando corretamente. Eles são importantes porque ajudam os programadores a detectar e corrigir rapidamente problemas em seu código, garantindo que o software final seja de alta qualidade.

## Como fazer:

Os testes podem ser escritos no Fish Shell usando a estrutura de testes integrada, que é conhecida como `fish unittest`. Aqui está um exemplo simples de como escrever um teste para verificar se uma função `add` retorna a soma correta de dois números:

```
fish unittest add_test
function add
  echo $1 + $2 | bc
end

add_test "Should return correct sum" 5 2; and status 0; and eq $output 7
```

A primeira linha `fish unittest add_test` define o nome do teste e `function add` define a função `add` a ser testada. Em seguida, é usada a ferramenta `bc` para realizar a adição e o resultado é armazenado em `output`. O último comando verifica se o teste teve êxito, fornecendo uma mensagem personalizada e verificando se o resultado é igual a 7.

## Mergulho Profundo:

Escrever testes é uma prática importante na programação moderna, pois ajuda a garantir que o código seja confiável e tenha poucos bugs. Existem outras ferramentas de teste disponíveis, como o `fish -n` para verificar a sintaxe e a `fish -c` para testar comandos individuais. Também é possível usar a linguagem de script `fish bundle` para escrever testes mais complexos e personalizados.

## Veja Também:

- [Documentação oficial do Fish Shell para testes](https://fishshell.com/docs/current/#fish-unittest)