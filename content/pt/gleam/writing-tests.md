---
title:                "Gleam: Escrevendo testes"
programming_language: "Gleam"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/writing-tests.md"
---

{{< edit_this_page >}}

## Por que escrever testes em Gleam?

Escrever testes é uma prática fundamental para garantir a qualidade e robustez do seu código em qualquer linguagem de programação. Em Gleam, isso não é diferente. Ao escrever testes, você pode identificar e corrigir erros em seu código de forma mais eficaz, evitando possíveis falhas e garantindo que seu código funcione corretamente em diferentes situações. Neste post, vamos explorar como escrever testes em Gleam e como isso pode beneficiar seu processo de desenvolvimento.

## Como escrever testes em Gleam

Para escrever testes em Gleam, é importante entender o conceito de módulos. Módulos são como pacotes que contêm funções e tipos específicos. Em Gleam, podemos criar um módulo de teste adicionando o sufixo "test" ao nome do módulo que estamos testando. Por exemplo, se o módulo principal se chama "exemplo", o módulo de teste correspondente será chamado de "exemplo_test".

Vamos ver um exemplo de como escrever um teste simples em Gleam:

```Gleam
pub fn somar(numero_um, numero_dois) {
  numero_um + numero_dois
}

pub fn teste_soma() {
  assert.equal(somar(2, 3), 5)
}
```

Neste exemplo, estamos testando a função somar que recebe dois números e retorna a sua soma. Ao adicionar a função de teste "teste_soma", estamos verificando se a função está retornando o resultado esperado. Para isso, utilizamos a função "assert.equal" que compara o resultado da função somar com o valor de referência 5. Caso os valores sejam diferentes, o teste falhará e uma mensagem de erro será exibida.

## Aprofundando-se em escrever testes em Gleam

Além do exemplo mostrado acima, existem outras maneiras de escrever testes em Gleam. Podemos adicionar anotações aos nossos testes para testar diferentes tipos de entradas e saídas. Essas anotações são chamadas de "propriedades" e nos permitem testar nosso código de forma mais abrangente. Também podemos utilizar as funções "assert.ok" e "assert.false" para verificar condições booleanas em nosso código.

É importante lembrar que os testes devem ser escritos em conjunto com o código principal. Isso significa que, à medida que você escreve novas funções, também deve escrever os testes correspondentes para garantir que tudo esteja funcionando corretamente.

## Veja também

- [Documentação oficial de testes em Gleam](https://gleam.run/tests/)
- [Guia de introdução a testes em Gleam](https://pragmatic.ly/introducing-au-testing-in-gleam-ff3a2fa2d980)
- [Exemplos de testes em Gleam](https://github.com/search?q=language%3Agleam+tests&type=Repositories&ref=advsearch&l=gleam&l=gleam) (em inglês)