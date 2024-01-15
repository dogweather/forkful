---
title:                "Escrevendo testes"
html_title:           "C#: Escrevendo testes"
simple_title:         "Escrevendo testes"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

## Por que

Escrever testes em um projeto de programação pode parecer um trabalho chato e desnecessário, mas na verdade é uma parte crucial do processo de desenvolvimento de software. Os testes ajudam a garantir que o código esteja funcionando corretamente e previnem futuros erros, economizando tempo e esforço no longo prazo.

## Como fazer

Escrever testes em C# é bastante simples, e pode ser feito usando a estrutura de testes integrada do Visual Studio ou uma biblioteca externa, como o NUnit. Veja abaixo um exemplo de teste de unidade simples usando NUnit:

```C#
using NUnit.Framework;

[TestFixture]
public class CalculatorTests
{
    [Test]
    public void When_AddMethod_IsCalled_ReturnsCorrectSum()
    {
        // Arrange
        int a = 10;
        int b = 5;
        Calculator calculator = new Calculator();

        // Act
        int result = calculator.Add(a, b);

        // Assert
        Assert.AreEqual(15, result);
    }
}
```

Neste exemplo, criamos um teste para a classe Calculator, que possui um método Add que recebe dois números e retorna a soma deles. Usando a sintaxe do NUnit, podemos testar se o resultado retornado pelo método é o esperado. 

## Deep Dive

Escrever bons testes em C# envolve entender os principais conceitos e tecnologias da linguagem. Alguns pontos a serem considerados são: 

- Testes de unidade vs testes de integração: enquanto os testes de unidade verificam a funcionalidade de uma unidade isolada de código, os testes de integração testam a interação entre diferentes unidades de código.
- Cobertura de código: para garantir que todos os caminhos do código estejam sendo testados, é importante medir a cobertura de código de seus testes.
- Testes automatizados: ter um conjunto de testes automatizados que podem ser executados sempre que houver uma alteração no código é essencial para garantir a qualidade do software.

Além disso, é importante seguir boas práticas ao escrever testes, como manter os testes independentes entre si e garantir que sejam facilmente entendidos pelos membros da equipe.

## Veja também

- [Documentação oficial do NUnit](https://docs.nunit.org/)
- [Melhores práticas para escrever testes em C#](https://dev.to/j_pohl/melhores-praticas-para-escrever-testes-em-c-36m2)
- [Artigo sobre testes automatizados em C#](https://www.c-sharpcorner.com/UploadFile/5b1f04/introduction-to-automated-testing-in-C-Sharp/)