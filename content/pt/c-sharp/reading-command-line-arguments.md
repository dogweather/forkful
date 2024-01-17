---
title:                "Lendo argumentos da linha de comando"
html_title:           "C#: Lendo argumentos da linha de comando"
simple_title:         "Lendo argumentos da linha de comando"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

O que & Porquê?

Ler argumentos da linha de comando é uma prática comum na programação, que consiste em obter informações passadas pelo usuário ao executar um programa. Isso permite que o programa seja mais interativo e personalizado, tornando a experiência do usuário mais fluída.

Os programadores utilizam essa técnica para permitir que seus programas sejam executados de diferentes formas, de acordo com as necessidades dos usuários. Além disso, ler argumentos da linha de comando permite que o código seja mais dinâmico e reutilizável, já que não há a necessidade de alterar manualmente variáveis ou configurações do programa a cada execução.

Como Fazer:

Ler argumentos da linha de comando em C# é uma tarefa simples e direta. Para isso, utilizamos o método `Main` com um parâmetro do tipo `string[]`, que representa uma coleção de argumentos passados na linha de comando.

Segue um exemplo de como ler dois argumentos (nome e idade) e imprimir uma mensagem personalizada usando esses dados:

```
C# 
static void Main(string[] args)
{
    // Lendo o primeiro argumento (nome)
    string nome = args[0];

    // Lendo o segundo argumento (idade)
    int idade = Convert.ToInt32(args[1]);

    // Criando a mensagem personalizada
    string mensagem = $"Olá {nome}, sua idade é {idade}";

    // Imprimindo a mensagem no console
    Console.WriteLine(mensagem);
}
```

Ao executar o programa com os argumentos "João" e "20", a saída será:

```
Olá João, sua idade é 20
```

É importante mencionar que os argumentos são lidos na ordem em que são passados, então é necessário que a quantidade e a ordem dos argumentos estejam corretas para que o programa funcione corretamente.

Mergulho Profundo:

A leitura de argumentos da linha de comando é uma técnica muito antiga e é utilizada em diversas linguagens de programação, não apenas em C#. A sua origem remonta aos primórdios da programação, quando os sistemas operacionais não tinham interfaces gráficas e os comandos eram executados apenas através da linha de comando.

Atualmente, existem outras formas de interação com o usuário, como interfaces gráficas e aplicativos móveis, mas a leitura de argumentos da linha de comando ainda é muito utilizada, principalmente em programas de linha de comando ou scripts automatizados.

Em C#, além do método `Main`, também é possível utilizar a classe `Environment` para obter informações sobre os argumentos passados na linha de comando, como por exemplo a quantidade total de argumentos e a string de argumentos completa.

Veja Também:

- [Documentação oficial do método Main (em inglês)](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/main-and-command-args/)
- [Exemplo prático de leitura de argumentos da linha de comando em C# (em inglês)](https://stackoverflow.com/questions/1629735/how-do-i-pass-command-line-arguments-to-a-visual-studio-vsto-add-in/2665915#2665915)