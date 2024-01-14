---
title:                "C#: Escrevendo testes"
simple_title:         "Escrevendo testes"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

## Por que escrever testes é importante

Escrever testes é uma prática essencial para garantir a qualidade do código em qualquer linguagem de programação, incluindo o C#. Eles servem como uma forma de validar se o nosso código está funcionando corretamente e também nos permitem identificar possíveis problemas antes que eles se tornem grandes problemas.

## Como escrever testes em C#

Para escrever testes em C#, é necessário utilizar uma estrutura de teste, como o NUnit ou o xUnit. Essas ferramentas permitem criar classes e métodos de teste que podem ser executados para verificar se o código está funcionando conforme o esperado.

Veja um exemplo de como escrever um teste simples usando o NUnit:

```C#
[Test]
public void TestSoma()
{
    // Arrange
    int a = 1;
    int b = 2;

    // Act
    int resultado = a + b;

    // Assert
    Assert.AreEqual(3, resultado);
}
```

Neste exemplo, temos um método de teste que realiza uma soma simples e verifica se o resultado é igual a 3. Caso contrário, o teste falhará e será necessário revisar o código.

## Aprofundando nos testes em C#

Além dos testes unitários, também é possível escrever testes de integração e testes funcionais em C#. Os testes de integração garantem que as diferentes partes do código estão se comunicando corretamente e os testes funcionais validam se a aplicação está funcionando conforme o esperado pelo usuário.

Também é importante lembrar de seguir boas práticas de testes, como nomear adequadamente os métodos de teste, manter os testes independentes uns dos outros e utilizar mocks para isolar o código que está sendo testado.

## Veja também

- [Documentação oficial do NUnit](https://nunit.org/)
- [Documentação oficial do xUnit](https://xunit.net/)
- [Artigo sobre testes em C# do Microsoft Docs](https://docs.microsoft.com/pt-br/dotnet/core/testing/)