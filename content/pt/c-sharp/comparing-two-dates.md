---
title:    "C#: Comparando duas datas"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que Comparar Datas é Importante na Programação

Comparar datas é uma tarefa comum na programação que pode ser útil em muitos cenários diferentes. Isso pode incluir a verificação de validade de uma licença, a ordenação de eventos em uma aplicação ou o cálculo do tempo decorrido entre duas datas.

## Como Comparar Datas em C#

```C#
// Criando duas datas para comparar
DateTime data1 = new DateTime(2020, 01, 15);
DateTime data2 = new DateTime(2020, 01, 20);

// Usando o método Compare para verificar a ordem das datas
int ordem = DateTime.Compare(data1, data2);

// Convertendo o resultado para texto
string resultado = (ordem < 0) ? "data1 vem antes de data2" : (ordem > 0) ? "data2 vem antes de data1" : "as datas são iguais";

// Imprimindo o resultado
Console.WriteLine(resultado);
// Saída: data1 vem antes de data2
```

O código acima cria duas datas e as compara usando o método `Compare` da classe `DateTime`. O resultado é atribuído a uma variável e, em seguida, é impresso na tela. Como podemos ver, a ordem é determinada pelo valor retornado pelo método, que é um inteiro negativo se a primeira data for anterior à segunda, um inteiro positivo se a segunda data for anterior à primeira e 0 se as datas forem iguais.

## Uma Análise mais Detalhada da Comparação de Datas

Ao comparar duas datas em C#, existem alguns fatores importantes a serem considerados. Primeiro, as datas devem estar no mesmo formato, ou seja, ambas devem ser do tipo `DateTime`. Além disso, o método `Compare` verifica o valor de cada componente das datas (ano, mês e dia) para determinar a ordem, portanto, datas com a mesma ordem, mas valores diferentes em um componente, podem resultar em uma comparação diferente.

Também é importante notar que o método `Compare` compara as informações de tempo das datas, o que pode levar a resultados inesperados se não forem levados em consideração. Por exemplo, duas datas podem estar na mesma ordem de data, mas com um intervalo de tempo significativo entre elas. Para evitar isso, é possível usar o método `Date`, que remove a informação de tempo da data antes de fazer a comparação.

## Veja Também

- [Documentação da Classe DateTime (em inglês)](https://docs.microsoft.com/pt-br/dotnet/api/system.datetime?view=netcore-3.1)
- [Tutorial sobre Comparação de Datas em C# (em inglês)](https://www.tutorialspoint.com/csharp/csharp_datetime_compare.htm)
- [Exemplo de Comparação de Datas em C# (em inglês)](https://www.techonthenet.com/csharp/date_time/compare_datetime.php)