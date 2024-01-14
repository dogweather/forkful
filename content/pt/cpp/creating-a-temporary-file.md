---
title:                "C++: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Porquê Criar um Ficheiro Temporário em C++

Criar um ficheiro temporário é uma técnica útil frequentemente usada em programação C++. Isto permite-nos armazenar dados de forma temporária durante a execução do nosso programa, e é especialmente útil quando precisamos de guardar dados temporários em memória sem alterar o nosso ficheiro original. Neste artigo, vamos mergulhar no mundo dos ficheiros temporários em C++ e aprender como os podemos criar.

## Como Criar um Ficheiro Temporário em C++

Criar um ficheiro temporário pode ser feito facilmente com o uso da biblioteca padrão C++ `fstream`. Em primeiro lugar, precisamos de incluir a biblioteca no nosso código.

```C++
#include <fstream> 
```

Em seguida, podemos usar a função `tmpfile()` para criar o nosso ficheiro temporário. Esta função retorna um apontador para o nosso ficheiro temporário, que podemos usar para escrever e ler dados.

```C++
FILE* tempFile = tmpfile();
```

Agora que temos o nosso ficheiro temporário, podemos escrever e ler dados como se estivéssemos a trabalhar com um ficheiro normal. Aqui está um exemplo simples de como escrever e ler uma linha de texto no nosso ficheiro temporário:

```C++
// Escrever no ficheiro temporário
fprintf(tempFile, "Olá mundo!");

// Ler do ficheiro temporário
char buffer[100];
fgets(buffer, 100, tempFile);
```

Depois de terminarmos de usar o nosso ficheiro temporário, é importante fechá-lo utilizando a função `fclose()`.

```C++
fclose(tempFile);
```

E voilà, agora temos um ficheiro temporário criado e podemos usá-lo para armazenar dados temporariamente durante a execução do nosso programa.

## Profundando no Mundo dos Ficheiros Temporários

Os ficheiros temporários não são apenas úteis para armazenar dados temporários, mas também para ajudar a gerir recursos no nosso programa. Quando criamos um ficheiro temporário, o sistema operativo atribui-lhe automaticamente um nome exclusivo e garante que este é eliminado automaticamente no final da execução do nosso programa.

Um aspeto importante a ter em conta é que, por vezes, o nosso programa pode terminar inesperadamente ou de forma abrupta, o que pode deixar o nosso ficheiro temporário pendente no sistema. É por isso que é importante verificar se o ficheiro temporário foi eliminado no início da execução do nosso programa e, caso contrário, certificar-nos de que é eliminado corretamente utilizando a função `remove()`.

## Vê Também

Se quiseres saber mais sobre a criação e uso de ficheiros temporários em C++, podes consultar estes recursos úteis:

- [Documentação da biblioteca fstream em cppreference.com](https://en.cppreference.com/w/cpp/header/fstream)
- [Como criar e utilizar ficheiros temporários em C++](https://www.tutorialspoint.com/how-to-create-and-use-temporary-files-in-cplusplus)
- [Vantagens e desvantagens de utilizar ficheiros temporários em C++](https://www.educative.io/edpresso/advantages-and-disadvantages-of-using-temporary-files-in-cplusplus)

Esperamos que este artigo te tenha dado uma melhor compreensão sobre a criação e uso de ficheiros temporários em C++. Agora estás pronto para usá-los em teus próprios programas e tirar proveito dos seus benefícios. Boa sorte!