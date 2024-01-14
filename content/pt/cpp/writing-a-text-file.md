---
title:                "C++: Escrevendo um arquivo de texto"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Por que escrever um arquivo de texto em C++

Se você é novo na programação em C++, pode estar se perguntando por que seria necessário escrever um arquivo de texto. Afinal de contas, existem muitas outras formas de armazenar informações em um programa. No entanto, há muitas situações em que escrever um arquivo de texto pode ser muito útil. Vamos explorar algumas delas.

# Como escrever um arquivo de texto em C++

Para escrever um arquivo de texto em C++, primeiro precisamos abrir um objeto de fluxo de arquivo usando a biblioteca padrão `fstream`. Podemos então usar o operador de inserção (`<<`) para enviar informações para o objeto de fluxo e, por fim, fechar o arquivo para salvar as alterações. Vamos ver um exemplo:

```C++
#include <iostream>
#include <fstream>
using namespace std;

int main() {
    ofstream arquivo; // Objeto de fluxo de arquivo para escrita
    arquivo.open("meu_arquivo.txt"); // Abre ou cria um arquivo chamado "meu_arquivo.txt"
    arquivo << "Este é um arquivo de texto gerado por C++"; // Insere texto no arquivo
    arquivo.close(); // Fecha o arquivo
    return 0;
}
```

Após a execução deste código, você deve ver um novo arquivo de texto chamado "meu_arquivo.txt" no mesmo diretório que o seu programa. Ao abri-lo, você encontrará o texto inserido no código, confirmando que o arquivo foi escrito corretamente.

# Deep Dive: Escrevendo um arquivo de texto com mais detalhes

Existem muitas outras coisas que podemos fazer ao escrever um arquivo de texto em C++. Por exemplo, podemos formatar a saída usando a manipulação de fluxo, escrever em várias linhas, ou mesmo adicionar informações de variáveis em tempo real. Você também pode adicionar múltiplos objetos de fluxo para escrever em diferentes arquivos simultaneamente. Para se aprofundar ainda mais, recomenda-se consultar a documentação da biblioteca `fstream` e experimentar com diferentes métodos e técnicas de escrita de arquivos de texto.

# Veja também

- [Tutorial C++: Criando e escrevendo em arquivos de texto](https://www.cplusplus.com/doc/tutorial/files/)
- [Documentação da biblioteca `fstream` em cplusplus.com](https://www.cplusplus.com/reference/fstream/)
- [Outras opções de gerenciamento de arquivos em C++](https://www.techiedelight.com/working-with-files-cpp/)