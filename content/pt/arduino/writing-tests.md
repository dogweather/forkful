---
title:                "Arduino: Escrevendo testes"
programming_language: "Arduino"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/writing-tests.md"
---

{{< edit_this_page >}}

## Por que Escrever Testes para Arduino?

Quando você está programando para Arduino, pode parecer tentador pular a etapa de escrever testes. Afinal, você pode simplesmente carregar o código no seu Arduino e verificar se funciona, certo? No entanto, escrever testes pode economizar tempo e evitar dores de cabeça no futuro. Você pode verificar se o seu código funciona corretamente antes de carregá-lo no Arduino e também pode mantê-lo atualizado com testes automatizados.

## Como Escrever Testes para Arduino

Escrever testes para Arduino é bastante simples. Você pode usar a biblioteca de teste "ArduinoUnit" disponível no gerenciador de bibliotecas da IDE do Arduino. Rode o seguinte código para instalar a biblioteca:

```Arduino
#include <ArduinoUnit.h> 
```

Em seguida, você pode escrever seus próprios testes criando uma função "test()" e usando os comandos "assert" fornecidos pela biblioteca "ArduinoUnit". Estes comandos podem verificar se o resultado do seu código é o esperado. Por exemplo:

```Arduino
void test() {
  assertEqual(2 + 2, 4);
}
```

Quando você carrega e executa o código, ele retorna o resultado dos testes em sua janela Serial, indicando se eles foram aprovados ou falharam. Isso permite que você detecte e corrija erros antes de carregar o código no seu Arduino.

## Aprofundando nos Testes para Arduino

Além de testar seu código com "asserts", você também pode adicionar mais complexidade aos seus testes usando a biblioteca "ArduinoUnit". Por exemplo, você pode criar objetos de teste e verificar suas propriedades ou até mesmo simular entradas externas para o seu Arduino. Isso permite que você verifique o comportamento do código em diferentes cenários e situações.

Além disso, é importante lembrar que você deve escrever testes para cada nova funcionalidade adicionada ao seu código. Isso ajuda a garantir que o código antigo não seja quebrado durante o processo de desenvolvimento.

## Veja Também

- [Documentação da Biblioteca ArduinoUnit](https://github.com/mmurdoch/arduinounit)

- [Tutorial de Testes para Arduino](http://arduinounit.sourceforge.net/)
 
- [Exemplos de Testes para Arduino](https://github.com/mmurdoch/arduinounit/wiki/More-Sample-Code)