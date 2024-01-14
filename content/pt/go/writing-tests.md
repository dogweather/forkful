---
title:    "Go: Escrevendo testes"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/go/writing-tests.md"
---

{{< edit_this_page >}}

## Por que escrever testes em Go?

Escrever testes é uma parte importante do processo de desenvolvimento de software em Go. Testes ajudam a validar o código e garantir que o mesmo funcione corretamente. Além disso, eles também permitem aos desenvolvedores fazer alterações no código com mais confiança, sabendo que os testes vão detectar quaisquer erros ou falhas.

## Como escrever testes em Go

Escrever testes em Go é bastante simples e pode ser feito usando o pacote de teste padrão `testing`. Veja um exemplo abaixo:

```Go
package main

import (
	"fmt"
	"testing"
)

func soma(a int, b int) int {
	return a + b
}

func TestSoma(t *testing.T) {
	resultado := soma(2, 3)
	esperado := 5
	if resultado != esperado {
		t.Errorf("Soma incorreta. Esperado: %d, obtido: %d", esperado, resultado)
	}
}
```

No exemplo acima, criamos uma função `soma` que retorna a soma de dois números e um teste `TestSoma` para validar a sua saída. Podemos executar este teste usando o comando `go test` e se tudo estiver correto, será exibida uma mensagem de sucesso. Caso contrário, será exibida uma mensagem de erro indicando o que deu errado.

Outro aspecto importante a ser lembrado ao escrever testes em Go é seguir a convenção de nomenclatura `Test{nome da função a ser testada}` para os testes e `Example{nome da função a ser testada}` para exemplos. Isso garante que o Go reconheça automaticamente os testes e exemplos ao executar o comando `go test`.

## Mais sobre testes em Go

Escrever testes em Go não se resume apenas ao exemplo simples que mostramos acima. Existem várias ferramentas e recursos disponíveis para facilitar e melhorar a qualidade dos testes. Alguns desses recursos incluem:

- Pacote `testing/quick`: fornece funções para gerar valores aleatórios e testar funções de forma rápida e fácil.
- Cobertura de código: o Go possui uma ferramenta integrada para gerar relatórios de cobertura de código, que podem ser usados ​​para identificar áreas de código que não estão sendo testadas.
- Testes de benchmarking: o pacote `testing` também fornece recursos para executar testes de benchmarking e medir o desempenho da sua função.

É importante lembrar que escrever testes de qualidade é uma habilidade que requer prática e aprendizado contínuo. Portanto, fique à vontade para explorar mais sobre o assunto e experimentar diferentes técnicas e abordagens para encontrar a que funciona melhor para você e seu código.

## Veja também

- Documentação oficial do pacote `testing`: https://golang.org/pkg/testing/
- Tutorial sobre testes em Go: https://medium.com/@povilasve/go-advanced-tips-tricks-a872503ac859
- Artigo sobre cobertura de código em Go: https://medium.com/rungo/unit-testing-made-easy-in-go-25077669318
- Exemplo de testes de benchmarking em Go: https://blog.golang.org/benchmarks