---
title:                "Trabalhando com csv"
html_title:           "C++: Trabalhando com csv"
simple_title:         "Trabalhando com csv"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/working-with-csv.md"
---

{{< edit_this_page >}}

## Por que

Trabalhar com arquivos CSV (Comma-Separated Values) é uma tarefa comum para muitos desenvolvedores de software, especialmente aqueles que trabalham com dados. CSV é um formato simples e eficiente para armazenar e transmitir dados tabulares, como planilhas, que podem ser facilmente lidos e manipulados em muitas linguagens de programação, incluindo C++.

## Como Fazer

Para trabalhar com CSV em C++, é necessário primeiro incluir a biblioteca fstream, que permite a leitura e gravação de arquivos. Em seguida, podemos usar a função getline() para ler cada linha do arquivo CSV e a estrutura de dados vector para armazenar os dados lidos. Veja um exemplo abaixo:

```C++
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>

using namespace std;

int main() {
    // Abrindo o arquivo CSV em modo de leitura
    ifstream file("dados.csv");
    
    // Verificando se o arquivo foi aberto corretamente
    if (!file.is_open()) {
        cout << "Não foi possível abrir o arquivo!" << endl;
        return 0;
    }
    
    // Vetor para armazenar os dados lidos
    vector<vector<string>> dados;
    
    // Variável para armazenar cada linha do arquivo
    string linha;
    
    // Lendo cada linha do arquivo
    while (getline(file, linha)) {
        // Usando stringstream para separar cada elemento da linha
        stringstream ss(linha);
        // Vetor para armazenar cada elemento da linha
        vector<string> elementos;
        // Variável para armazenar cada elemento
        string elemento;
        
        // Lendo cada elemento separado por vírgula
        while (getline(ss, elemento, ',')) {
            // Adicionando o elemento ao vetor
            elementos.push_back(elemento);
        }
        
        // Adicionando o vetor de elementos ao vetor de dados
        dados.push_back(elementos);
    }
    
    // Exibindo os dados lidos
    for (auto linha : dados) {
        for (auto elemento : linha) {
            cout << elemento << " ";
        }
        cout << endl;
    }
    
    // Fechando o arquivo
    file.close();
    
    return 0;
}
```

Suponha que o arquivo CSV "dados.csv" contenha o seguinte conteúdo:

```
Nome,Sobrenome,Idade,Profissão
João,Silva,30,Programador
Maria,Santos,28,Analista de Dados
```

A saída do programa seria:

```
Nome Sobrenome Idade Profissão
João Silva 30 Programador
Maria Santos 28 Analista de Dados
```

E é isso! Agora você já pode ler e armazenar dados de um arquivo CSV em C++.

## Mergulho Profundo

Além da função getline() e da estrutura vector, a linguagem C++ oferece outras funções que podem ser úteis na manipulação de arquivos CSV, como a função ignore(), que pode ser usada para ignorar certa quantidade de caracteres em uma linha, e a função good(), que verifica se a leitura do arquivo foi bem-sucedida. Além disso, é importante se atentar às particularidades do formato CSV, como o tratamento de valores nulos e valores com espaços. É recomendado também verificar a documentação da biblioteca fstream para mais informações e funcionalidades.

## Veja Também

- [Documentação oficial da biblioteca fstream](https://en.cppreference.com/w/cpp/header/fstream)
- [Como ler e escrever arquivos em C++](https://www.geeksforgeeks.org/input-output-in-cpp/)
- [Tutorial sobre manipulação de CSV em C++](https://www.tutorialspoint.com/cplusplus-program-to-read-a-csv-file-and-store-the-data)