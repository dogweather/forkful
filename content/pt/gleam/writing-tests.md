---
title:    "Gleam: Escrevendo testes"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Por que escrever testes é importante para programadores iniciantes

Quando começamos a aprender a programar, muitas vezes sentimos a tentação de pular a etapa de escrever testes. Afinal, é mais divertido criar novas funções e ver nosso código funcionar do que escrever testes que parecem não servir para nada. Porém, a verdade é que escrever testes é uma parte fundamental do processo de programar e pode trazer muitos benefícios para desenvolvedores iniciantes.

## Como escrever testes em Gleam

Escrever testes é uma prática importante em qualquer linguagem de programação, e em Gleam isso não é diferente. Para começar, é preciso importar o módulo `testing` no seu arquivo de teste. Em seguida, é possível utilizar a função `describe` para agrupar testes com uma determinada descrição. Dentro de `describe`, é possível utilizar a função `test` para criar testes individuais. Veja um exemplo abaixo:

```Gleam
import testing

describe "Testes de adição" {

  test "2 + 2 deve ser igual a 4" {
    expect(2 + 2) |> to_equal(4)
  }
  
  test "10 + 5 deve ser igual a 15" {
    expect(10 + 5) |> to_equal(15)
  }
}
```

Ao rodar esse arquivo como um script, o resultado será:

```
Running tests...
✓ Testes de adição: 2 passed
```

## Aprofundando-se em testes em Gleam

Agora que já vimos como criar testes básicos em Gleam, é importante entender alguns conceitos importantes para escrever testes eficientes e eficazes. Um desses conceitos é a "assertividade", que se refere à habilidade do teste em garantir que o resultado esperado seja alcançado. Para alcançar uma boa assertividade, é importante testar diferentes casos, incluindo casos extremos e entradas inválidas.

Outro conceito importante é a cobertura de código, que se refere à porcentagem do código que é coberta por testes. É importante sempre buscar uma alta cobertura de código, uma vez que isso garante uma maior confiabilidade do código em geral.

Além disso, é importante se familiarizar com as funções de teste disponíveis no módulo `testing`, como `to_equal`, `to_be_true`, `to_be_nil`, entre outras. Essas funções permitem especificar o comportamento esperado do código em cada teste.

## Veja também
- [Documentação oficial sobre testes em Gleam](https://gleam.run/book/tour/testing.html)
- [Artigo do Gleam sobre a importância de escrever testes](https://gleam.run/news/testing.html)
- [Vídeo tutorial sobre testes em Gleam](https://www.youtube.com/watch?v=4zx3j0twl7M)