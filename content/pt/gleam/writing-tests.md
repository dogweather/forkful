---
title:    "Gleam: Escrevendo testes"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/writing-tests.md"
---

{{< edit_this_page >}}

## Por que escrever testes em Gleam?

Escrever testes é uma prática essencial para garantir a qualidade e a estabilidade do seu código em Gleam. Além disso, ao escrever testes, você pode detectar e corrigir erros antes que eles se tornem um problema real em produção.

## Como escrever testes em Gleam

Para escrever testes em Gleam, você pode usar a biblioteca padrão de teste [gleam_test](https://gleam.run/documentation/standard_library#gleam_test). Vamos dar uma olhada em um exemplo simples de um módulo de teste.

```
Gleam import List

Gleam test "Testar função de adição" {
  Gleam let numbers = [1, 2, 3]
  Gleam assert_eq(List.fold_left(numbers, 0, Int.add), 6)
  Gleam assert_eq(List.fold_left(numbers, 0, Int.multiply), 0)
}
```

Na primeira linha, importamos o módulo `List` para usar suas funções de lista. Em seguida, usamos a declaração `test` para definir um escopo para nossos testes. Dentro deste escopo, podemos definir variáveis e usar as funções de teste `assert_eq` para verificar se o resultado esperado é igual ao resultado real.

Para executar este teste, basta executar `gleam test` no seu diretório de projeto.

## Aprofundando-se em testes em Gleam

Além do exemplo básico acima, há muito mais que você pode fazer com testes em Gleam. Você pode usar a função `suite` para agrupar diferentes testes em uma suíte de testes, usar os módulos `String` e `Option` para facilitar a criação de casos de teste e mais. Para saber mais sobre como escrever testes em Gleam, consulte a documentação oficial.

## Veja também

- [Documentação da biblioteca padrão do Gleam](https://gleam.run/documentation/standard_library)
- [Exemplos de testes em Gleam](https://github.com/gleam-lang/gleam_rebar3_exemple/blob/master/test)
- [Guia de melhores práticas para escrever testes em Gleam](https://dev.to/jeuhenri/how-to-write-tests-in-gleam-1eob)