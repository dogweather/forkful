---
title:                "Calculando uma data no futuro ou passado"
html_title:           "Swift: Calculando uma data no futuro ou passado"
simple_title:         "Calculando uma data no futuro ou passado"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

Em programação, muitas vezes precisamos calcular datas no futuro ou no passado para realizar diferentes tarefas. Isso significa que temos que determinar uma data específica adicionando ou subtraindo um certo número de dias, semanas, meses ou anos de uma data existente. Isso pode ser usado para fins de agendamento de eventos, configuração de lembretes, cálculo de prazos, entre outros.

## O que e por quê?

Calcular uma data no futuro ou no passado é uma tarefa comum entre os programadores. Isso permite que criemos lógicas específicas para atender às necessidades de nossos aplicativos, sem ter que lidar manualmente com cada situação. Por exemplo, ao programar um aplicativo de comércio eletrônico, podemos querer exibir uma data estimada de entrega ao usuário, com base na data de compra e no tempo estimado de entrega.

## Como fazer:

Para calcular uma data no futuro ou no passado em Swift, podemos utilizar a classe `Calendar`, que possui o método `date(byAdding:to:)`. Este método recebe uma instância do tipo `DateComponents`, que possui propriedades para definir os componentes que queremos adicionar ou subtrair da data original. Podemos passar valores negativos para subtrair e valores positivos para adicionar.

```
// Criando uma data original
let dataOriginal = Date()

// Definindo uma instância de DateComponents para adicionar 5 dias à data original
var dataComponents = DateComponents()
dataComponents.day = 5

// Utilizando o método date(byAdding:to:) para obter a nova data
let dataFutura = Calendar.current.date(byAdding: dataComponents, to: dataOriginal)

// Imprimindo a nova data
print(dataFutura) // 5 dias no futuro
```

Podemos alterar os valores da instância `DateComponents` para adicionar ou subtrair outros componentes, como meses ou anos. O método `date(byAdding:to:)` também pode ser utilizado para calcular uma data no passado.

## Mergulho profundo:

Uma das principais razões para a criação do método `date(byAdding:to:)` foi o fato de o Swift ser uma linguagem de programação orientada a protocolos, o que permite que os desenvolvedores criem tipos personalizados que adotem o protocolo `Calendar`. Isso significa que é possível criar uma classe que funcione como um calendário personalizado e seja capaz de efetuar cálculos de datas no futuro e no passado de forma específica.

Além disso, existem outras formas de calcular datas no futuro ou no passado em Swift, como utilizando a classe `DateComponentsFormatter` ou a biblioteca de terceiros `DateToolsSwift`. No entanto, o método `date(byAdding:to:)` é a forma mais comummente utilizada e recomendada pela documentação oficial do Swift.

## Veja também:

- [Documentação oficial do Swift](https://developer.apple.com/documentation/foundation/calendar/2292622-date)
- [Como trabalhar com datas em Swift](https://www.raywenderlich.com/1606463-dates-and-calculations-in-swift)