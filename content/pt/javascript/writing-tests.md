---
title:                "Escrevendo testes"
html_title:           "Javascript: Escrevendo testes"
simple_title:         "Escrevendo testes"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/writing-tests.md"
---

{{< edit_this_page >}}

## O que & Por quê?

Escrever testes é uma prática comum entre os programadores para garantir a qualidade e funcionamento correto de seu código. Isso envolve a criação de pequenos trechos de código que testam diferentes partes da aplicação e verificam se os resultados esperados são obtidos. Os testes são importantes para detectar falhas e bugs em um estágio inicial e garantir que a aplicação funcionará corretamente quando utilizada pelo usuário final.

## Como fazer:

```Javascript
// Exemplo de teste simples
const square = (x) => {
  return x * x;
}

// Teste usando o framework Jest
test('Deve retornar 25 quando o número 5 for inserido', () => {
    expect(square(5)).toBe(25);
});
```

Neste exemplo, um teste simples é feito para uma função que calcula o quadrado de um número. Primeiro, definimos a função `square` que recebe um parâmetro `x` e retorna o quadrado desse número. Em seguida, usamos o framework de testes Jest para criar um teste que verifica se ao passar o número 5 como argumento para a função, o resultado é 25. Ao executar este teste, se o resultado for diferente de 25, ele irá falhar.

## Aprofundando:

Escrever testes não é uma prática nova, mas com o aumento da popularidade do desenvolvimento ágil e do TDD (Test Driven Development), sua importância tem sido amplamente reconhecida pela comunidade de programação. Existem também outras formas de testar o código, como o BDD (Behavior Driven Development) e o ATDD (Acceptance Test Driven Development). Cada abordagem tem suas próprias vantagens e desvantagens, mas todas têm o mesmo objetivo de garantir a qualidade do código.

## Veja também:

- [Por que escrever testes é tão importante?](https://www.publicaaweb.com.br/importancia-testes-software/)
- [Guia para iniciantes em automação de testes](https://blog.cedrosolucoes.com.br/testes-automatizados-primeiros-passos-para-iniciantes/)
- [Documentação do framework Jest](https://jestjs.io/docs/getting-started)