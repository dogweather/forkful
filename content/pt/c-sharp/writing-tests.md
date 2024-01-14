---
title:                "C#: Escrevendo testes"
programming_language: "C#"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

## Por Que

Se você é um desenvolvedor de software, provavelmente já ouviu falar sobre a importância de escrever testes para o seu código. Mas por que é realmente necessário fazer isso? A resposta é simples: escrever testes é uma forma eficiente de garantir que o seu código está funcionando corretamente e que qualquer alteração que você faça não vai afetar o funcionamento do seu programa.

## Como Fazer

Agora que você sabe por que é importante escrever testes, vamos ver como fazer isso em C#. A primeira coisa a se fazer é criar uma classe de teste. Por exemplo, vamos criar uma classe de teste para uma calculadora simples:

```C#
public class CalculadoraTeste
{
    [Fact]
    public void Somar_DeveRetornarSomaCorreta()
    {
        // Arrange
        var calculadora = new Calculadora();

        // Act
        var resultado = calculadora.Somar(2, 2);

        // Assert
        Assert.Equal(4, resultado);
    }
}
```

Neste exemplo, estamos utilizando a biblioteca de testes xUnit, mas você pode escolher qualquer biblioteca que preferir. Note que estamos utilizando o conceito de "Arrange-Act-Assert", que significa organizar os dados para o teste, executar a funcionalidade que será testada e finalmente verificar se o resultado está correto.

## Mergulho Profundo

Existem diversos tipos de testes que podem ser aplicados em um projeto C#, como testes unitários, de integração, de sistema, entre outros. Cada tipo possui uma finalidade específica e juntos, garantem um código mais robusto e confiável.

Além disso, é importante lembrar que a prática de escrever testes deve ser constante e não apenas uma etapa no início do desenvolvimento. Conforme o código é modificado, os testes também devem ser atualizados para refletir essas mudanças.

## Veja Também

 - [Documentação oficial do xUnit](https://xunit.net/)
 - [Artigo sobre testes unitários em C#](https://www.treinaweb.com.br/blog/testes-unitarios-com-c/)