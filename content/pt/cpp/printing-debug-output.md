---
title:                "C++: Imprimindo saída de depuração"
simple_title:         "Imprimindo saída de depuração"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que

Há muitas maneiras de acompanhar e depurar o seu código em C++, mas a impressão de saída de depuração é uma das mais populares. Isso permite que os desenvolvedores vejam informações sobre como o programa está executando e identifiquem possíveis problemas.

## Como fazer

Para imprimir saída de depuração em seu código C++, você pode usar a função "cout" da biblioteca "iostream". Basta incluir a linha "#include <iostream>" no início do seu código e, em seguida, usar o operador de inserção "<<" para imprimir as informações desejadas. Aqui está um exemplo simples:

```
#include <iostream>
using namespace std;

int main() {
    int a = 5;
    int b = 10;
    cout << "A soma de " << a << " e " << b << " é " << (a+b) << endl;
    return 0;
}
```

Este código imprimirá a frase "A soma de 5 e 10 é 15" na saída do console. Você também pode imprimir informações de variáveis ​​e usar formatação de saída, como o número de casas decimais a serem mostradas. Para obter mais detalhes sobre como imprimir saída de depuração em C++, consulte este [artigo](https://www.tutorialspoint.com/cplusplus/cpp_debugging.htm) ou este [guia](http://www.cplusplus.com/reference/iostream/cout/).

## Mergulho profundo

Além de simplesmente imprimir informações durante a depuração, a saída do console também pode ser útil para rastrear o fluxo de um programa e identificar possíveis erros. Por exemplo, você pode imprimir mensagens em diferentes partes do código para ver onde o programa está executando e verificar os valores das variáveis ​​em cada etapa.

Outra vantagem da impressão de saída de depuração é poder deixar o código "comentado" sem realmente usar comentários. Isso pode ser especialmente útil para identificar linhas específicas de código que podem estar causando problemas.

## Veja também

- [Tutorial de Impressão de Saída de Depuração em C++](https://www.tutorialspoint.com/cplusplus/cpp_debugging.htm)
- [Referência da função "cout" em C++](http://www.cplusplus.com/reference/iostream/cout/)