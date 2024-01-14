---
title:    "C#: Lendo argumentos da linha de comando"
keywords: ["C#"]
---

{{< edit_this_page >}}

Por que ler argumentos de linha de comando é importante?

Ler argumentos de linha de comando é uma habilidade importante para programadores de C#. Isso permite que seu programa receba inputs dinamicamente do usuário, tornando-o mais versátil e interativo. Além disso, a leitura de argumentos de linha de comando pode tornar seu código mais eficiente, já que você não precisa recompilar toda vez que os inputs mudarem.

Como ler argumentos de linha de comando em C#

Para ler argumentos de linha de comando em C#, usamos o método `Main()` e a classe `Environment`. Primeiro, declaramos um array de strings que receberá os argumentos passados na linha de comando. Em seguida, usamos o método `GetCommandLineArgs()` da classe `Environment` para atribuir os argumentos ao array. Por fim, podemos acessar cada argumento pelo seu índice no array.

```C#
static void Main(string[] args)
{
    string[] argumentos = Environment.GetCommandLineArgs();

    Console.WriteLine("Os argumentos de linha de comando são:");

    // Começamos com o índice 1, pois o índice 0 sempre é o caminho do executável
    for (int i = 1; i < argumentos.Length; i++)
    {
        Console.WriteLine($"{i}: {argumentos[i]}");
    }
}
```

Supondo que o nome do arquivo compilado seja `meuPrograma.exe`, podemos executá-lo passando argumentos na linha de comando da seguinte forma:

```
meuPrograma.exe argumento1 argumento2 argumento3
```

A saída seria:

```
Os argumentos de linha de comando são:
1: argumento1
2: argumento2
3: argumento3
```

Agora que sabemos o básico de como ler argumentos de linha de comando, vamos nos aprofundar um pouco mais.

Mergulho em leitura de argumentos de linha de comando

Além do método `GetCommandLineArgs()`, a classe `Environment` também possui outros métodos úteis para lidar com argumentos de linha de comando. Por exemplo, o método `GetCommandLine()` retorna uma string contendo todos os argumentos passados na linha de comando, enquanto `GetCommandLineArgs().Length` nos dá o número total de argumentos.

É importante notar que, por padrão, todos os argumentos passados na linha de comando são tratados como strings. Isso significa que, se você precisar usar os argumentos em cálculos ou operações numéricas, será necessário convertê-los adequadamente antes.

Além disso, é possível passar argumentos com espaços na linha de comando usando aspas duplas. Por exemplo:

```
meuPrograma.exe "primeiro argumento" "segundo argumento" "terceiro argumento"
```

Nesse caso, todos os argumentos seriam lidos e tratados como uma única string, incluindo os espaços.

Veja também

- Documentação oficial da classe Environment: https://docs.microsoft.com/en-us/dotnet/api/system.environment?view=netframework-4.8
- Leitura e validação de argumentos de linha de comando em C#: https://www.dotnetperls.com/command-line-arguments