---
title:    "C#: Escrevendo testes"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

## Por que escrever testes em C#?

Escrever testes é uma parte fundamental do processo de desenvolvimento de software em C#. Além de garantir que o código esteja funcionando corretamente, os testes também ajudam a identificar e corrigir bugs e a manter a qualidade do código. Além disso, escrever testes pode ajudar a economizar tempo e esforço no longo prazo, pois evita problemas e retrabalho em etapas posteriores do desenvolvimento.

## Como escrever testes em C#

Para escrever testes em C#, utilizamos a biblioteca de testes NUnit. Primeiramente, é necessário instalar o pacote NUnit através do gerenciador de pacotes NuGet no Visual Studio. Depois de instalado, podemos começar a escrever nossos testes.

Para criar uma classe de teste, basta adicionar o atributo [TestFixture] no topo da classe e adicionar o atributo [Test] antes dos métodos de teste. Dentro do método de teste, utilizamos a classe Assert para verificar se o resultado esperado é igual ao resultado obtido. Veja um exemplo abaixo:

```C#
[TestFixture]
public class CalculadoraTeste
{
    [Test]
    public void TesteSoma()
    {
        // Arrange
        var calculadora = new Calculadora();
        var num1 = 5;
        var num2 = 2;
        // Act
        var resultado = calculadora.Soma(num1, num2);
        // Assert
        Assert.AreEqual(7, resultado);
    }
}
```

## Aprofundando-se em testes em C#

Além do NUnit, existem outras bibliotecas de testes em C# como xUnit e MSTest. Cada uma possui suas próprias características e vantagens, por isso é importante explorar e escolher a que melhor se encaixa em seu projeto.

Também é importante lembrar de seguir boas práticas ao escrever testes, como manter os testes simples e independentes, utilizar nomes descritivos para os métodos de teste e cobrir casos de uso diferentes para garantir uma boa cobertura de testes.

Outra prática importante é a utilização de testes de unidade e testes de integração. Os testes de unidade verificam a funcionalidade de uma unidade específica de código, enquanto os testes de integração testam a interação de diferentes unidades de código.

## Veja também

- [Documentação do NUnit](https://docs.nunit.org/)
- [Artigo sobre testes em C# da Microsoft](https://docs.microsoft.com/pt-br/dotnet/core/testing/)
- [Tutorial de testes em C# da DevMedia](https://www.devmedia.com.br/testes-unitarios-em-csharp/27130)