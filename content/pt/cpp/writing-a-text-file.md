---
title:    "C++: Escrevendo um arquivo de texto"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por que escrever um arquivo de texto em C++?

Se você é um programador iniciante ou experiente em C++, provavelmente já se deparou com a necessidade de escrever um arquivo de texto em algum momento. Mas por que fazemos isso?

Escrever um arquivo de texto é útil quando queremos armazenar dados ou informações permanentemente, ou seja, mesmo quando o programa é finalizado, esses dados ainda estarão lá para serem acessados em uma próxima execução. Isso é especialmente útil para dados que não podem ser perdidos, como configurações de um programa ou resultados de uma análise.

## Como escrever um arquivo de texto em C++

Escrever um arquivo de texto em C++ é uma tarefa relativamente simples. Primeiro, precisamos incluir a biblioteca fstream para trabalhar com arquivos de texto. Em seguida, devemos criar um objeto do tipo ofstream e utilizá-lo para escrever em um arquivo específico.

```
#include <fstream>

using namespace std;

int main() {
    ofstream arquivo("exemplo.txt"); //cria o arquivo

    //escreve no arquivo
    arquivo << "Este é um exemplo de arquivo de texto" << endl;
    arquivo << "Podemos escrever informações aqui" << endl;

    arquivo.close(); //fecha o arquivo

    return 0;
}
```

Se executarmos esse código, um novo arquivo chamado "exemplo.txt" será criado e irá conter as informações que especificamos.

## Mergulho Profundo

Além de escrever simplesmente texto em um arquivo, também é possível formatar e organizar os dados de forma mais clara e legível. Podemos usar comandos como setw(), setfill(), setprecision(), entre outros, para formatar números e strings em uma saída de texto.

Também é importante lembrar de verificar se o arquivo foi aberto com sucesso antes de tentar escrever nele. Podemos fazer isso verificando o valor booleano do objeto ofstream depois de criá-lo.

## Veja também

- [Tutorial básico sobre arquivos em C++ (em inglês)](https://www.cplusplus.com/doc/tutorial/files/)
- [Documentação da classe ofstream (em inglês)](https://www.cplusplus.com/reference/fstream/ofstream/)
- [Mais exemplos de escrita de arquivos em C++ (em inglês)](https://www.geeksforgeeks.org/writing-text-file-c/)