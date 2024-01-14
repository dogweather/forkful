---
title:                "C++: Escrevendo para o erro padrão"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que escrever para o erro padrão?

Ao escrever em C++, é comum interagir com o usuário do programa através da entrada e saídas padrão. No entanto, às vezes é necessário enviar mensagens de erro para o usuário, e é aí que entra o erro padrão. Ao escrever para o erro padrão, é possível comunicar ao usuário quaisquer problemas ou exceções que ocorram durante a execução do programa, facilitando a identificação e solução de possíveis erros.

## Como escrever para o erro padrão

Para escrever para o erro padrão em C++, é necessário utilizar a função `std::cerr`, que faz parte da biblioteca padrão `<iostream>`. Esta função aceita argumentos do tipo `char*`, que são as mensagens de erro que se deseja enviar para o usuário. Veja um exemplo de código abaixo:

```C++
#include <iostream>

int main() {
  int idade;

  // Pedindo a idade do usuário
  std::cout << "Informe sua idade: ";
  std::cin >> idade;

  // Verificando se a idade é válida
  if (idade < 0) {
    // Imprimindo mensagem de erro para o usuário
    std::cerr << "Idade inválida! A idade não pode ser negativa." << std::endl;
  }

  return 0;
}
```

Para executar este código, basta compilar e rodar o programa normalmente. Porém, quando a idade digitada for um valor negativo, o usuário receberá a mensagem de erro no terminal.

## Aprofundando-se na escrita para o erro padrão

Além da função `std::cerr`, também é possível utilizar `std::clog` para escrever para o erro padrão. A diferença é que `std::cerr` é desalocada imediatamente após a execução, enquanto `std::clog` é armazenada em um buffer antes de ser desalocada. Em geral, as mensagens de erro devem ser enviadas através de `std::cerr`, enquanto `std::clog` pode ser utilizada para mensagens de depuração ou informações importantes.

Outro ponto relevante é que a função `std::cerr` é um objeto do tipo `ostream`, assim como `std::cout`, e portanto é possível utilizar os mesmos operadores de formatação para escrever mensagens personalizadas para o usuário.

## Veja também

- [Documentação oficial da função std::cerr](https://www.cplusplus.com/reference/iostream/cerr/)
- [Tutorial sobre entrada e saída em C++](https://www.geeksforgeeks.org/basic-input-output-c/)
- [Guia completo sobre formatação de saída em C++](https://www.tutorialspoint.com/cplusplus/cpp_data_output.htm)