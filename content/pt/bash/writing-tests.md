---
title:                "Escrevendo testes"
html_title:           "Bash: Escrevendo testes"
simple_title:         "Escrevendo testes"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/writing-tests.md"
---

{{< edit_this_page >}}

## O que e para que serve?

Escrever testes em programação é uma técnica utilizada pelos programadores para garantir que o código que estão desenvolvendo está funcionando corretamente. Esses testes são pequenos programas criados para verificar se o código produz os resultados esperados e identificar possíveis erros.

Muitas vezes, os programadores escrevem testes antes mesmo de iniciar a escrita do código em si, pois isso ajuda a definir melhor o propósito do código e quais resultados ele deve produzir.

## Como fazer:

Para escrever testes em Bash, você pode utilizar o comando `assert` seguido de um comando ou expressão ao qual você quer testar. Por exemplo:

```Bash
assert 5 -gt 3
```

Este comando verifica se o número 5 é maior que 3 e retorna um código de saída `0` caso o teste seja bem sucedido. Caso contrário, o código de saída será diferente de `0`, indicando que o teste falhou.

## Mergulho profundo:

A prática de escrever testes não é exclusiva do Bash, mas sim uma técnica adotada por programadores em diversas linguagens de programação. Ela se originou no movimento de desenvolvimento de software chamado "Programação Extrema" (XP), que preza por agilidade e qualidade no processo de desenvolvimento.

Existem outras ferramentas que também podem ser utilizadas para escrever testes em Bash, como o `shUnit2` e o `bats`. Além disso, você pode implementar testes automatizados em diferentes áreas, como testes unitários, de integração e de aceitação.

## Veja também:

- [Documentação oficial do Bash](https://www.gnu.org/software/bash/)
- [Documentação oficial do shUnit2](https://github.com/kward/shunit2)
- [Documentação oficial do bats](https://github.com/bats-core/bats-core)