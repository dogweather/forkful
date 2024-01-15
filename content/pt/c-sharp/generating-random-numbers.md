---
title:                "Gerando números aleatórios"
html_title:           "C#: Gerando números aleatórios"
simple_title:         "Gerando números aleatórios"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por que gerar números aleatórios?

Há muitas razões pelas quais você pode querer gerar números aleatórios em seu programa C#. Pode ser para criar senhas seguras, gerar dados de teste para fins de depuração ou até mesmo para criar elementos aleatórios em um jogo.

## Como fazer

Você pode gerar números aleatórios de duas maneiras diferentes em C#: usando a classe `Random` ou a função estática `Random.Next()`. Ambos os métodos funcionam da mesma forma, mas a classe `Random` oferece mais funcionalidades. Aqui está um exemplo de como gerar um número aleatório entre 1 e 100:

```C#
Random random = new Random();
int numeroAleatorio = random.Next(1, 101);
Console.WriteLine("Número aleatório: " + numeroAleatorio);
```

O código acima irá gerar e imprimir na tela um número aleatório entre 1 e 100. Você pode modificar os parâmetros da função `Next()` para gerar números em um intervalo diferente.

## Profundando no assunto

Ao gerar números aleatórios, é importante entender que eles não são realmente aleatórios. Eles são gerados através de algoritmos que utilizam um número chamado "seed" (semente) para calcular o próximo número. Se você não especificar uma semente, a classe `Random` irá utilizar o milissegundo atual como semente.

Se você deseja gerar uma sequência de números que pareçam mais aleatórios, você pode especificar uma semente manualmente ou usar a função `RandomGuid()` para gerar um número aleatório baseado no relógio do sistema.

Outra coisa importante a saber é que, se você criar uma nova instância da classe `Random` em um intervalo de tempo muito curto, é provável que ela gere os mesmos números novamente. Portanto, é recomendável manter uma única instância de `Random` durante todo o tempo de execução do programa.

## Veja também

- [Documentação oficial do C# sobre a classe Random](https://docs.microsoft.com/en-us/dotnet/api/system.random)
- [Tutorial da Microsoft sobre como gerar números aleatórios em C#](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/concepts/linq/generating-random-numbers)
- [Diferenças entre a classe Random e a função Random.Next() em C#](https://stackoverflow.com/questions/924350/difference-between-randomnext-and-random-nextdouble)