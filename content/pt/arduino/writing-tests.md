---
title:                "Arduino: Escrevendo testes"
simple_title:         "Escrevendo testes"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/writing-tests.md"
---

{{< edit_this_page >}}

## Por que escrever testes é importante em programação com Arduino?

Escrever testes é uma prática importante em programação com Arduino porque garante que o seu código funcione corretamente. Além disso, os testes permitem que você detecte e corrija erros antes mesmo de implementar o seu projeto, economizando tempo e garantindo um código mais confiável.

## Como escrever testes em programação com Arduino?

Para escrever testes em programação com Arduino, é necessário seguir os seguintes passos:

1. Identifique as partes do código que precisam ser testadas.
2. Crie uma função de teste para cada parte do código que deseja testar.
3. Dentro de cada função de teste, escreva o código que irá testar a funcionalidade desejada.
4. Utilize a função `ASSERT()` para verificar se os resultados obtidos são os esperados.
5. Compile o código e verifique se todos os testes foram aprovados.

Um exemplo de código de teste em Arduino seria:

```
Arduino int x = 10;
int y = 5;
// Função de teste para multiplicação
void testMultiplicacao() {
  int resultado = x * y; // Resultado esperado: 50
  ASSERT(resultado == 50); // Teste falhará se o resultado for diferente de 50
}
```

Ao compilar e rodar esses testes, caso o resultado seja diferente de 50, o teste irá falhar e você poderá identificar onde está o erro e corrigi-lo.

## Aprofundando mais nos testes em programação com Arduino

Escrever testes em programação com Arduino também pode ser muito útil para garantir que todas as funcionalidades do seu projeto estejam funcionando corretamente. Além disso, ao escrever testes de unidade, você pode testar cada função isoladamente, facilitando a identificação e correção de possíveis erros.

Outra técnica interessante é utilizar testes de integração, onde você pode testar a interação entre diferentes partes do código. Isso é especialmente importante para projetos mais complexos, onde várias funções dependem umas das outras.

Além disso, existem também ferramentas e bibliotecas específicas para escrever e rodar testes em programas com Arduino, como a ArduinoUnit e a ArduinoUnit-C, que facilitam a criação e execução de testes. Com essas ferramentas, é possível criar testes mais avançados, como testes de aceitação e testes de estresse.

## Confira também

- [Tutorial: Teste de Unidade com Arduino](https://www.arduino.cc/en/Guide/Introduction/#toc3)
- [ArduinoUnit: Biblioteca para escrever testes em Arduino](https://www.arduino.cc/reference/en/libraries/arduinounit/)
- [ArduinoUnit-C: Biblioteca para escrever testes em C para Arduino](https://www.arduino.cc/reference/en/libraries/arduinounit-c/)
- [Introdução aos testes de integração](https://www.freecodecamp.org/news/integration-testing-in-arduino-6896eb21d66/)