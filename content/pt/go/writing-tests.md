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

## O que é e por que fazer?

Escrever testes é um processo em que os programadores criam pequenos programas para testar o código que eles escreveram. Isso garante que o código funcione corretamente e evita bugs em seu software. É uma prática comum em programação e é altamente recomendado para garantir a qualidade do código.

## Como fazer:

Para escrever testes em Go, é necessário usar o pacote `testing`. Dentro desse pacote, podemos usar a função `Test` para criar nossos testes. Aqui está um exemplo simples de como escrevê-los:

```
package main

import "testing"

func TestSum(t *testing.T) {
	result := Sum(2, 3)
	if result != 5 {
		t.Errorf("Expected 5, got %d", result)
	}
}
```

Esse teste espera que a função `Sum` some corretamente os dois números e retorne 5. Se o resultado for diferente de 5, o teste falhará e mostrará uma mensagem de erro. É importante lembrar que o nome da função de teste deve começar com a palavra "Test" e receber um parâmetro do tipo `*testing.T`.

Podemos também usar a função `Run` para agrupar vários testes em um único lugar. Isso é útil para organizar melhor os testes e facilitar a execução de todos eles de uma vez. Aqui está um exemplo:

```
func TestAll(t *testing.T) {
	t.Run("Testing Sum", TestSum)
	t.Run("Testing Subtraction", TestSub)
	t.Run("Testing Multiplication", TestMult)
	t.Run("Testing Division", TestDiv)
}
```

Essa seria a função `main` completa para rodar todos os testes que escrevemos:

```
func main() {
    t.Run("Tests", TestAll)
}
```

## Mais detalhes:

O teste de código é uma prática antiga que remonta aos primeiros dias da programação. No passado, os testes eram realizados manualmente, mas com a evolução da tecnologia, surgiram ferramentas e frameworks para automatizar esse processo. Em Go, o pacote `testing` é uma dessas ferramentas.

Existem também outras formas de escrever testes em Go, como usando o pacote `testify`, que oferece uma sintaxe mais legível e fácil de usar. No entanto, o pacote `testing` já é uma ferramenta poderosa o suficiente para a maioria dos casos.

Se você quiser se aprofundar mais em como escrever testes em Go, recomendo explorar a documentação oficial e experimentar diferentes técnicas e ferramentas.

## Veja também:

- Documentação oficial do pacote `testing` em Go: https://golang.org/pkg/testing/
- Pacote `testify` para escrever testes em Go: https://github.com/stretchr/testify