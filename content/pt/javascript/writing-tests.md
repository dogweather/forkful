---
title:                "Javascript: Escrevendo testes"
simple_title:         "Escrevendo testes"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/writing-tests.md"
---

{{< edit_this_page >}}

## Por que escrever testes?

Ao escrever um código em Javascript, muitas vezes nos deparamos com a necessidade de realizar testes para garantir que o nosso código está funcionando corretamente. Isso é importante para evitar possíveis erros e bugs em nosso código, além de garantir a qualidade e confiabilidade do mesmo.

## Como fazer isso?

Para escrever testes em Javascript, podemos usar uma ferramenta chamada Jest. Com o Jest, podemos criar testes automatizados que irão verificar se o código se comporta conforme o esperado. Abaixo, segue um exemplo de código e sua saída utilizando o Jest:

```Javascript
// Função para retornar o dobro de um número
function dobrarNumero(numero) {
  return numero * 2;
}

// Teste para verificar se a função retorna o dobro corretamente
it('deve dobrar corretamente', () => {
  expect(dobrarNumero(5)).toBe(10);
});
```

Saída esperada:

```
✔ deve dobrar corretamente
```

Dessa forma, podemos garantir que a nossa função está retornando o dobro corretamente, evitando possíveis erros no futuro.

## Aprofundando mais

Escrever testes pode parecer um processo complexo e demorado, mas na verdade, quando bem feito, pode economizar tempo e evitar dores de cabeça. Existem várias técnicas e boas práticas para escrever testes efetivos, como o TDD (Test Driven Development) e BDD (Behavior Driven Development).

Além disso, é importante entender a diferença entre testes unitários, que verificam uma função ou módulo específico, e testes de integração, que testam a integração entre diferentes partes do código.

Outro conceito importante é o de cobertura de testes, que indica a porcentagem do código que foi testado. É recomendado ter uma boa cobertura de testes para garantir que todas as partes do código estão sendo verificadas adequadamente.

## Veja também

- [Documentação do Jest](https://jestjs.io/pt-BR/)
- [Test Driven Development: Guia para Iniciantes](https://blog.geekhunter.com.br/test-driven-development/)
- [5 Princípios para Escrever Testes Angular Eficazes](https://blog.cubos.io/testes-angular-eficazes/)
- [Integração Contínua e Testes Automatizados: Qual a Importância?](https://blog.locaweb.com.br/desenvolvimento-web/integracao-continua-testes-automatizados/)