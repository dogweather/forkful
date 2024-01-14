---
title:    "Go: Escrevendo testes"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Por que escrever testes em Go?

Escrever testes é uma prática essencial para garantir a qualidade e funcionamento correto de um código em qualquer linguagem de programação. Em Go, isso não é diferente. Ao escrever testes, você pode ter mais confiança em suas implementações e também facilitar a manutenção e modificação do código no futuro.

## Como escrever testes em Go

Para escrever testes em Go, devemos utilizar o pacote "testing", que já vem incluído na linguagem. Este pacote oferece um conjunto de funções para criação e execução de testes. Por exemplo:

```Go
func TestSoma(t *testing.T) {
    resultado := soma(2,3)
    if resultado != 5 {
        t.Errorf("Soma incorreta. Esperado: 5, Obtido: %d", resultado)
    }
}
```

Neste exemplo, estamos testando a função "soma", que deve retornar a soma de dois números inteiros. Utilizamos a função "TestSoma", que recebe como parâmetro um ponteiro para o tipo "testing.T", que é utilizado para reportar erros e falhas nos testes. Dentro do teste, chamamos a função "soma" com dois valores e, utilizando a função "Errorf" do pacote testing, verificamos se o resultado obtido é o esperado. Se não for, o teste irá falhar e uma mensagem de erro será exibida.

## Dive Profundo: Escrevendo testes eficientes

Além de saber como escrever testes em Go, é importante saber como escrevê-los de forma eficiente. Algumas boas práticas incluem:
- Testar todos os cenários possíveis, incluindo os casos de erro
- Utilizar a função "Fatal" em vez de "Error" para erros críticos, pois a primeira irá encerrar o teste imediatamente
- Utilizar o pacote "subtests" para testar uma série de casos com apenas um teste

Com essas dicas, você pode escrever testes mais completos e eficientes para garantir a qualidade do seu código.

## Veja também
- [Documentação oficial do pacote "testing" em Go](https://golang.org/pkg/testing/)
- [Artigo sobre "testes unitários" em Go](https://medium.com/trainingcenter/testando-com-go-unit-tests-293bfc3e4e27)
- [Tutorial de testes em Go com exemplos práticos](https://www.calhoun.io/intro-to-benchmarking-and-profiling-in-go-part-2/)