---
title:                "Criando um arquivo temporário"
html_title:           "C++: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que

Criar um arquivo temporário é uma prática comum em programação, especialmente quando precisamos armazenar dados temporários ou realizar operações em um arquivo sem alterar o original. Isso também pode ser útil em situações em que precisamos garantir que os dados não sejam perdidos durante o processo de execução do programa.

## Como Fazer

Para criar um arquivo temporário em C++, podemos utilizar a função `tmpfile()` da biblioteca `cstdio`. Esta função criará um novo arquivo temporário e o abrirá para escrita e leitura. Veja um exemplo de código abaixo:

```C++
#include <cstdio>

int main() {

  // Cria um arquivo temporário
  FILE *temp = tmpfile();
  
  // Verifica se o arquivo foi criado com sucesso
  if (temp == NULL) {
    // Se houver um erro, exibe uma mensagem e encerra o programa
    printf("Erro ao criar o arquivo temporário.");
    return 1;
  }
  
  // Escreve uma string no arquivo
  fputs("Este é um arquivo temporário.", temp);
  
  // Lê o conteúdo do arquivo e exibe na tela
  char str[100];
  rewind(temp); // Define o ponteiro de leitura no início do arquivo
  fgets(str, 100, temp);
  printf("Conteúdo do arquivo temporário: %s", str);
  
  // Fecha e remove o arquivo temporário
  fclose(temp);
  
  return 0;
}

```

O código acima criará um arquivo temporário e o preencherá com a string "Este é um arquivo temporário.". Em seguida, ele lerá o conteúdo do arquivo e exibirá na tela. Por fim, o arquivo é fechado e removido automaticamente.

## Deep Dive

Além da função `tmpfile()`, há outras maneiras de criar arquivos temporários em C++. Outra opção é utilizar a função `tmpnam()`, que criará um nome para o arquivo temporário a ser criado. É importante ressaltar que o nome gerado pode não ser único, então é necessário tomar cuidado para garantir que o arquivo criado seja exclusivo.

Uma alternativa para gerar nomes únicos de arquivo temporário é utilizar a função `mkstemp()`, que criará o arquivo com um nome aleatório e também garante que ele seja único. No entanto, é necessário especificar um prefixo para o nome do arquivo, que pode ser obtido com a função `tmpnam()`.

Existem também bibliotecas de terceiros que oferecem recursos mais avançados para manipulação de arquivos temporários em C++, como a Boost.Filesystem e a Poco TemporaryFile.

## Veja Também

- [Documentação oficial do C++ sobre a função `tmpfile()`](https://devdocs.io/cpp/io/c/tmpfile)
- [Tutorial sobre criação de arquivos temporários em C++](https://www.geeksforgeeks.org/tmpfile-function-in-c-cpp/)
- [Exemplos de uso da biblioteca Boost.Filesystem](https://www.boost.org/doc/libs/1_77_0/libs/filesystem/doc/reference.html#Temporary-Files)