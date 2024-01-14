---
title:    "C++: Lendo um arquivo de texto"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que?

Ler e escrever arquivos de texto é uma parte fundamental da programação em C++. É uma maneira de armazenar e acessar dados importantes para o seu programa. Aprender a ler um arquivo de texto pode ser muito útil para ler informações como configurações de usuário, dados de entrada, ou até mesmo arquivos de texto simples. Neste artigo, veremos como ler um arquivo de texto em C++ e alguns recursos avançados para explorar ainda mais.

## Como fazer

Para ler um arquivo de texto em C++, primeiro precisamos incluir a biblioteca padrão "fstream" no topo do nosso código. Em seguida, criamos um objeto "ifstream" (input file stream) e usamos o método "open" para abrir o arquivo de texto que desejamos ler. 

Dentro dos blocos de código abaixo, podemos ver como ler um arquivo de texto linha por linha e exibir cada linha no console. Para fins de demonstração, usaremos um arquivo de texto chamado "input.txt" que contém algumas frases simples.

```
#include <iostream>
#include <fstream>

using namespace std;

int main() {

  ifstream inputFile;
  inputFile.open("input.txt");

  // verificando se o arquivo foi aberto com sucesso
  if (!inputFile) {
    cout << "Erro ao abrir o arquivo!";
    return 1;
  }

  // variável para armazenar cada linha do arquivo
  string line;

  // loop para ler cada linha do arquivo
  while (getline(inputFile, line)) {
    // exibindo a linha no console
    cout << line << endl;
  }

  // fechando o arquivo
  inputFile.close();

  return 0;
}
```

O resultado dessa execução é a impressão de cada linha do arquivo "input.txt" no console:

```
Eu amo programar em C++!
Aprender a ler arquivos de texto é muito útil.
Espero que este artigo tenha sido útil para você.
```

No exemplo acima, usamos o método "getline" para ler linha por linha do arquivo. Mas também é possível ler todo o conteúdo de uma vez usando o método "read" e armazenando o conteúdo em uma variável de string. Você pode explorar mais métodos e recursos para ler arquivos de texto em C++ na seção a seguir.

## Mergulho profundo

Ler um arquivo de texto é apenas uma das muitas operações que podemos fazer em C++. Existem muitos recursos avançados para explorar, como manipulação de arquivos binários, tratamento de erros, formatação de saída, entre outros. Além disso, também é importante entender como fechar um arquivo corretamente após a leitura, para evitar perda de dados ou acessos incorretos no sistema.

Uma dica importante é sempre verificar se o arquivo foi aberto com sucesso antes de começar a ler ou escrever. Isso evitará a ocorrência de erros e problemas no seu programa. Além disso, é recomendável usar um loop para ler o arquivo, pois assim podemos manipular cada linha ou bloco separadamente, conforme necessário.

## Veja também

- [Documentação oficial do C++ para trabalhar com arquivos de texto](https://en.cppreference.com/w/cpp/io/basic_ifstream)
- [Guia completo sobre manipulação de arquivos em C++](https://www.learncpp.com/cpp-tutorial/186-basic-file-io/)
- [Tutorial em vídeo sobre leitura e escrita de arquivos de texto em C++](https://www.youtube.com/watch?v=4jb4AYEyhR0)