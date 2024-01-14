---
title:    "C++: Escrevendo para o erro padrão"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Por que escrever para o erro padrão?

Ao desenvolver um programa em C ++, é comum encontrar erros que precisam ser debugados. Ao escrever para o erro padrão, você pode criar mensagens de erro personalizadas que ajudam a identificar e corrigir esses problemas com mais facilidade.

## Como fazer?

Para escrever para o erro padrão em C ++, você precisa usar a função `cerr` junto com o operador de inserção `<<`. Aqui está um exemplo simples:

```C++
#include <iostream>

using namespace std;

int main()
{
    //Exemplo de mensagem de erro personalizada
    cerr << "Erro: O valor inserido é inválido." << endl;
}
```

A saída seria: `Erro: O valor inserido é inválido.` O operador `<<` pode ser usado para inserir variáveis, valores ou até mesmo expressões em sua mensagem de erro.

## Profundidade do assunto

A função `cerr` é uma função de saída de fluxo de erro que é automaticamente conectada ao erro padrão do sistema. Isso significa que quando você escreve para `cerr`, a mensagem é direcionada para o terminal ou console e não para a saída normal do programa.

No entanto, é importante notar que `cerr` não interrompe a execução do programa. Isso significa que se houver vários erros, as mensagens serão impressas em ordem, mas o programa continuará sendo executado.

Além disso, `cerr` não tem nenhuma formatação específica. Isso significa que, se você quiser adicionar informações extras, como o número da linha onde ocorreu o erro, você precisará incluí-los manualmente em sua mensagem de erro.

## Veja também

- [Documentação da função cerr em C ++](https://www.cplusplus.com/reference/iostream/cerr/)
- [Como depurar erros C ++ usando a função cerr](https://www.geeksforgeeks.org/using-cerr-instead-endl-debugging-c/)
- [Diferença entre cout e cerr em C ++](https://stackoverflow.com/questions/1058747/what-is-the-difference-between-cout-and-cerr)