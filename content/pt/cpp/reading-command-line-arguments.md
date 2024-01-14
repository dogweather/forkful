---
title:                "C++: Lendo argumentos da linha de comando"
simple_title:         "Lendo argumentos da linha de comando"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por que ler argumentos da linha de comando?

Ler argumentos da linha de comando é uma habilidade fundamental para qualquer programador em C++. Com ela, é possível interagir com o usuário e criar programas mais dinâmicos e flexíveis. Além disso, a leitura de argumentos da linha de comando permite que o usuário passe informações essenciais para a execução do programa, como arquivos de entrada e configurações específicas.

## Como Fazer

Ler argumentos da linha de comando em C++ é bastante simples e pode ser feito usando a função `main()`. Basta declarar os parâmetros `argc` e `argv` que representam, respectivamente, o número de argumentos passados e um vetor que armazena os argumentos em strings.

```C++
int main(int argc, char *argv[]) {
    // código para processar os argumentos aqui
    return 0;
}
```

Agora, é possível acessar cada argumento passado pelo usuário através do vetor `argv` e executar as ações desejadas. Por exemplo, se o usuário passar o argumento "-h" para exibir a ajuda do programa, podemos fazer:

```C++
if (strcmp(argv[1], "-h") == 0) {
    cout << "Este programa realiza operações matemáticas." << endl;
    cout << "Opções disponíveis: " << endl;
    cout << "-s : calcular soma" << endl;
    cout << "-m : calcular multiplicação" << endl;
    cout << "-p : calcular potência" << endl;
}
```

A saída do programa seria:

```
$ ./calculadora -h
Este programa realiza operações matemáticas.
Opções disponíveis:
-s : calcular soma
-m : calcular multiplicação
-p : calcular potência
```

## Aprofundando-se

Além dos argumentos que o usuário pode passar, também existem argumentos obrigatórios que precisam ser tratados pelo programa. Para isso, é necessário verificar se o número de argumentos `argc` é maior ou igual ao número mínimo esperado e, caso contrário, informar ao usuário que algo está faltando.

Outro ponto importante é considerar possíveis erros de digitação ou entrada incorreta de informações pelo usuário. Para isso, é recomendado o uso de validação e tratamento de exceções para garantir que o programa não tenha falhas inesperadas.

Além disso, é importante entender a diferença entre argumentos de linha de comando com e sem opção. Argumentos com opção são aqueles que possuem um valor associado, como por exemplo o argumento `-n 10` em que 10 é o valor da opção `n`. Já argumentos sem opção são apenas marcadores que indicam a execução de uma determinada tarefa.

## Ver também

- [Tutorial de command line arguments em C++](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/)
- [Documentação sobre a função main() em C++](https://www.cplusplus.com/articles/yAqpX9L8/)
- [Exemplos de uso de argumentos da linha de comando em C++](https://www.techiedelight.com/command-line-arguments-cpp/)