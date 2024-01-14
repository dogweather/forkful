---
title:    "Kotlin: Escrevendo testes"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/writing-tests.md"
---

{{< edit_this_page >}}

## Por que escrever testes é importante?

Escrever testes é uma parte essencial do processo de desenvolvimento de software. Testes unitários e de integração garantem que o código está funcionando corretamente e que as mudanças feitas não quebraram funcionalidades existentes. Além disso, ter uma boa cobertura de testes aumenta a confiabilidade do código e facilita a manutenção no futuro.

## Como escrever testes usando Kotlin?

Para escrever testes em Kotlin, primeiro é preciso incluir a biblioteca de testes no arquivo gradle:
```
dependencies {
    testImplementation 'junit:junit:4.12'
}
```
Em seguida, podemos criar uma classe de teste e adicionar o annotation `@Test` antes do método que desejamos testar. Por exemplo:
```
class CalculadoraTest{

    @Test
    fun testarSoma(){
        val calc = Calculadora()
        val resultado = calc.soma(2,3)
        assertEquals(5, resultado)
    }
}
```

Neste exemplo, criamos uma classe de teste `CalculadoraTest` e adicionamos o método `testarSoma` com o annotation `@Test`. Dentro deste método, criamos uma instância da classe `Calculadora` e chamamos o método `soma` com dois números. Então, usamos o método `assertEquals` para verificar se o resultado é o esperado. Se o resultado não for igual a 5, o teste falhará.

É importante notar que há diversas bibliotecas de teste disponíveis para Kotlin, então cabe ao desenvolvedor escolher a que melhor se adequa ao seu projeto.

## Mais informações sobre escrever testes em Kotlin

Além dos testes unitários e de integração, existem outras formas de escrever testes em Kotlin. Por exemplo, testes de interface de usuário usando a biblioteca Espresso ou testes funcionais usando a biblioteca Robolectric.

Outro ponto importante é seguir boas práticas de teste, como manter os testes independentes e testar apenas uma funcionalidade em cada teste, para facilitar a identificação e correção de problemas.

## Veja também

- [Documentação oficial do JUnit](https://junit.org/junit5/docs/current/user-guide/)
- [Tutorial do Android TDD com Kotlin e Espresso](https://blog.mindorks.com/android-testing-tutorial-1-android-studio-best-practices-12910788abc7?gi=8d7bfbf7678e)
- [GitHub da biblioteca Robolectric](https://github.com/robolectric/robolectric)