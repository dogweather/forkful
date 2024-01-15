---
title:                "Escrevendo testes"
html_title:           "Go: Escrevendo testes"
simple_title:         "Escrevendo testes"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/writing-tests.md"
---

{{< edit_this_page >}}

## Por que escrever testes é importante?

Escrever testes é uma prática muito valiosa na programação. Quando você escreve testes, está garantindo que o seu código funciona corretamente e pode ser alterado com confiança no futuro. Além disso, testes ajudam a comunicar a funcionalidade do seu código para outros desenvolvedores de forma clara e concisa.

## Como escrever testes em Go

Para escrever testes eficientes em Go, é necessário utilizar a biblioteca de testes integrada, a "testing". Primeiro, importe a biblioteca utilizando o comando `import "testing"`. Depois, crie funções iniciando com a palavra chave `Test` e o nome do método a ser testado. Dentro da função, utilize os métodos `t.Error()` ou `t.Fail()` para indicar falhas nos testes. A seguir, um exemplo de teste para uma função que soma dois números inteiros:

```
func TestSoma(t *testing.T) {
  resultado := soma(2, 3)
  if resultado != 5 {
    t.Fail()
  }
}
```

Você pode rodar os testes utilizando o comando `go test` no seu terminal. Caso todos os testes passem, você receberá a mensagem `ok`, caso contrário, receberá os erros específicos que devem ser corrigidos.

## Mergulho profundo

Ao escrever testes em Go, é importante seguir algumas boas práticas. Nomeie suas funções de testes de forma descritiva e clara para facilitar a leitura e entendimento dos outros desenvolvedores. Além disso, é possível utilizar a função `t.Skip()` para ignorar certos testes em determinadas situações. Por fim, teste cada função individualmente e evite testar várias funcionalidades em um único teste.

## Veja também

- [Documentação oficial do pacote de testes em Go](https://golang.org/pkg/testing/)
- [Tutorial de como escrever testes em Go](https://golang.org/doc/tutorial/add-a-test)
- [Boas práticas para escrever testes em Go](https://medium.com/@betuik/9-go-best-practices-that-you-should-always-follow-7f5d4333c4b6)