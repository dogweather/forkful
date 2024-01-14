---
title:                "C++: Escrevendo testes"
programming_language: "C++"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/writing-tests.md"
---

{{< edit_this_page >}}

## Por que escrever testes é importante para o desenvolvimento de software?

Escrever testes é uma parte crucial do processo de desenvolvimento de software. Ao escrever testes, podemos garantir que nosso código funciona corretamente e que quaisquer alterações futuras não quebrarão o código existente. Além disso, testes bem escritos podem ajudar a identificar e corrigir erros antes mesmo da implementação do código em produção, economizando tempo e recursos.

## Como escrever testes em C++?

Para escrever testes em C++, podemos usar a biblioteca de testes padrão do C++ chamada "gtest". Primeiro, devemos incluir o arquivo de cabeçalho "gtest.h" em nosso código. Em seguida, podemos definir nossas funções de teste usando a macro "TEST (test_suite_name, test_name)". Com a ajuda de várias macros, podemos verificar as condições de teste desejadas e gerar a saída correspondente.

Um exemplo simples de função de teste pode ser o seguinte:

```C++
TEST (MathTest, SumTest) {
    EXPECT_EQ(5, sum(2, 3)); // verifica se 2 + 3 é igual a 5
    EXPECT_EQ(0, sum(5, -5)); // verifica se 5 + (-5) é igual a 0
}
```

Aqui, o primeiro parâmetro da macro "TEST" é o nome da suíte de testes e o segundo parâmetro é o nome do teste. As macros "EXPECT_EQ" e "EXPECT_EQ" são usadas para verificar se o valor retornado por nossa função de teste corresponde ao valor esperado. Se todos os testes passarem com sucesso, a saída será "OK", caso contrário, a saída mostrará quais testes falharam e qual era o valor esperado.

## Considerações ao escrever testes

Ao escrever testes em C++, é importante garantir que nossos testes sejam independentes uns dos outros e também sejam fáceis de entender e manter. Devemos incluir testes para cenários de sucesso, bem como para possíveis cenários de erro. Além disso, devemos lembrar de verificar todas as condições relevantes para o nosso código, para garantir uma cobertura de teste adequada.

Outra consideração importante é seguir boas práticas de programação enquanto escrevemos nosso código de teste. Isso inclui manter nossos testes pequenos e específicos, usar nomes descritivos para funções de teste e adicionar anotações para tornar a leitura e a manutenção dos testes mais fáceis.

## Veja também

- [Documentação oficial do gtest](https://github.com/google/googletest)
- [Tutorial de testes em C++](https://www.guru99.com/cpp-unit-testing.html)
- [Melhores práticas de testes em C++](https://www.experitest.com/blog/best-practices-for-unit-testing-in-c/)