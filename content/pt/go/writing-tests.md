---
title:                "Go: Escrevendo Testes"
simple_title:         "Escrevendo Testes"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/writing-tests.md"
---

{{< edit_this_page >}}

## Por que escrever testes em Go?

Escrever testes para o nosso código não é apenas uma boa prática, mas também uma parte essencial do processo de desenvolvimento. Além de garantir que o nosso código funciona corretamente, os testes também nos ajudam a identificar e prevenir bugs e a realizar refatorações com mais facilidade. Em resumo, escrever testes nos ajuda a garantir que o nosso código seja confiável e de alta qualidade.

## Como escrever testes em Go?

Em Go, os testes são escritos utilizando o pacote `testing`, que possui três funções principais: `Test`, `Bench` e `Example`. Vamos dar uma olhada em um exemplo de teste simples:

```Go
func Soma(a, b int) int {
	return a + b
}

func TestSoma(t *testing.T) {
	resultado := Soma(2, 3)
	if resultado != 5 {
		t.Errorf("A soma de 2 e 3 deveria ser igual a 5, mas o resultado foi %v", resultado)
	}
}
```

No código acima, definimos uma função `Soma` que recebe dois números inteiros e retorna a soma deles. Em seguida, escrevemos o nosso teste na função `TestSoma`, onde chamamos a função `Soma` e verificamos se o resultado é o esperado utilizando o método `t.Errorf`. Para executar os nossos testes, basta rodar o comando `go test` no terminal.

Além disso, também podemos usar o método `t.Fatal` para interromper os testes caso algo inesperado aconteça, e o método `t.Helper` para indicar que aquela é uma função auxiliar de teste.

## Mergulho profundo em escrever testes

Escrever testes em Go pode ser muito mais do que apenas verificar se uma função retorna o resultado esperado. Podemos também utilizar a biblioteca `httptest` para testar a funcionalidade de endpoints HTTP, a biblioteca `sync` para testar concorrência e a biblioteca `reflect` para testar estruturas de dados.

Além disso, é possível criar testes de benchmark utilizando a função `Benchmark` e os métodos `b.N` e `b.ResetTimer`. E para testar a cobertura dos nossos testes, podemos usar a flag `-cover` no comando `go test`.

## Veja também

- [Documentação oficial de testes em Go](https://golang.org/pkg/testing/)
- [Tutorial de testes em Go](https://golang.org/doc/tutorial/add-a-test)
- [Exemplos de testes em Go](https://github.com/golang/go/tree/master/src/testing/tests)