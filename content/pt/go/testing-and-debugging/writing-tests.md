---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:15:05.347340-07:00
description: "Escrever testes em Go envolve criar peda\xE7os de c\xF3digo pequenos\
  \ e gerenci\xE1veis que validam a funcionalidade e o comportamento da sua aplica\xE7\
  \xE3o.\u2026"
lastmod: 2024-02-19 22:05:05.129910
model: gpt-4-0125-preview
summary: "Escrever testes em Go envolve criar peda\xE7os de c\xF3digo pequenos e gerenci\xE1\
  veis que validam a funcionalidade e o comportamento da sua aplica\xE7\xE3o.\u2026"
title: Escrevendo testes
---

{{< edit_this_page >}}

## O Quê & Por Quê?

Escrever testes em Go envolve criar pedaços de código pequenos e gerenciáveis que validam a funcionalidade e o comportamento da sua aplicação. Programadores escrevem testes para garantir que seu código funcione como esperado sob diversas condições, para facilitar o refatoramento e para ajudar a prevenir regressões.

## Como fazer:

Em Go, os testes são tipicamente escritos no mesmo pacote do código que eles testam. Arquivos contendo testes são nomeados com o sufixo `_test.go`. Os testes são funções que recebem um ponteiro para o objeto testing.T (do pacote `testing`) como argumento, e sinalizam falha chamando métodos como `t.Fail()`, `t.Errorf()`, etc.

Exemplo de um teste simples para uma função `Add` definida em `math.go`:
```go
// math.go
package math

func Add(x, y int) int {
    return x + y
}
```

Arquivo de teste `math_test.go`:
```go
package math

import "testing"

func TestAdd(t *testing.T) {
    resultado := Add(1, 2)
    esperado := 3
    if resultado != esperado {
        t.Errorf("Add(1, 2) = %d; quer %d", resultado, esperado)
    }
}
```

Execute seus testes com o comando `go test` no mesmo diretório dos seus arquivos de teste. Um exemplo de saída indicando um teste que passou se pareceria com:

```
PASS
ok      example.com/my/math 0.002s
```

Para testes baseados em tabelas, que permitem testar de forma eficiente várias combinações de entrada e saída, defina uma fatia de structs representando os casos de teste:

```go
func TestAddTableDriven(t *testing.T) {
    var testes = []struct {
        x        int
        y        int
        esperado int
    }{
        {1, 2, 3},
        {2, 3, 5},
        {-1, -2, -3},
    }

    for _, tt := range testes {
        nomeTeste := fmt.Sprintf("%d+%d", tt.x, tt.y)
        t.Run(nomeTeste, func(t *testing.T) {
            res := Add(tt.x, tt.y)
            if res != tt.esperado {
                t.Errorf("obteve %d, quer %d", res, tt.esperado)
            }
        })
    }
}
```

## Aprofundamento

O framework de testes do Go, introduzido no Go 1 juntamente com a própria linguagem, foi projetado para se integrar perfeitamente com a cadeia de ferramentas do Go, refletindo a ênfase do Go na simplicidade e eficiência no desenvolvimento de software. Ao contrário de alguns frameworks de teste em outras linguagens que dependem de bibliotecas externas ou configurações complexas, o pacote integrado `testing` do Go oferece uma maneira direta de escrever e executar testes.

Um aspecto interessante da abordagem do Go para testes é o princípio de convenção sobre configuração que adota, como o padrão de nomenclatura de arquivo (`_test.go`) e o uso de funcionalidades da biblioteca padrão em vez de dependências externas. Esta abordagem minimalista incentiva os desenvolvedores a escrever testes, já que a barreira de entrada é baixa.

Embora as facilidades de teste integradas do Go cubram muito terreno, existem cenários onde ferramentas ou frameworks de terceiros podem oferecer mais funcionalidades, como geração de mock, fuzz testing ou testes no estilo de desenvolvimento orientado por comportamento (BDD). Bibliotecas populares como Testify ou GoMock complementam as capacidades de teste padrão do Go, oferecendo afirmações mais expressivas ou capacidades de geração de mock, que podem ser particularmente úteis em aplicações complexas com muitas dependências.

Apesar da existência dessas alternativas, o pacote de teste padrão do Go permanece a pedra angular para testes em Go devido à sua simplicidade, desempenho e integração estreita com a linguagem e a cadeia de ferramentas. Independentemente de os desenvolvedores optarem por aumentá-lo com ferramentas de terceiros ou não, o framework de testes do Go fornece uma base sólida para garantir a qualidade e a confiabilidade do código.
