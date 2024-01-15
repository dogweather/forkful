---
title:                "Escrevendo um arquivo de texto"
html_title:           "C++: Escrevendo um arquivo de texto"
simple_title:         "Escrevendo um arquivo de texto"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por que

Escrever arquivos de texto pode ser uma tarefa muito útil para os programadores. Ele permite armazenar dados de forma persistente, acessá-los facilmente e compartilhá-los com outros usuários.

## Como Fazer

Para escrever um arquivo de texto usando C++, você precisa seguir alguns passos simples:

- Primeiro, você precisará incluir a biblioteca "fstream" no seu código.
- Em seguida, crie um objeto do tipo "ofstream" para abrir o arquivo em modo de escrita.
- Use o operador de inserção (<<) para escrever o conteúdo no arquivo.
- Finalmente, feche o arquivo usando o método "close()" do objeto.

Veja um exemplo de código em C++:

```C++
#include <fstream>
using namespace std;

int main() {
    // Criando o objeto do tipo "ofstream"
    ofstream arquivo;
    // Abertura do arquivo em modo de escrita
    arquivo.open("arquivo.txt");
    // Escrevendo no arquivo
    arquivo << "Este é um exemplo de texto que será escrito no arquivo.";
    // Fechando o arquivo
    arquivo.close();
    return 0;
}
```

O código acima criará um arquivo chamado "arquivo.txt" no mesmo diretório onde o seu código está sendo executado, e escreverá o texto nele.

## Mergulho Profundo

Ao escrever um arquivo de texto usando C++, existem algumas coisas importantes a serem consideradas:

- O modo de abertura do arquivo: ao utilizar o método "open()", você pode especificar se deseja abrir o arquivo em modo de escrita, leitura ou ambos. Certifique-se de usar o modo correto para o que você deseja fazer.
- Formatação: ao escrever no arquivo, você pode usar caracteres de escape, como "\n" para pular linhas ou "\t" para inserir tabulações. Esses caracteres devem ser colocados dentro de aspas duplas, assim como o texto que você deseja escrever.
- Manipulação de erros: tenha em mente que o processo de escrita em um arquivo pode falhar por vários motivos, desde problemas com permissões até arquivos inexistentes. Portanto, é importante adicionar tratamentos de erro ao seu código para lidar com essas situações.

## Veja Também

- [C++: Trabalhando com Arquivos de Texto](https://www.cplusplus.com/doc/tutorial/files/)
- [Documentação da Biblioteca "fstream" do C++](https://www.cplusplus.com/reference/fstream/)
- [Tutorial de C++](https://www.devmedia.com.br/tutoriais/c-plus-plus/) (em português)