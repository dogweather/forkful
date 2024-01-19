---
title:                "Imprimindo saída de debug"
html_title:           "C#: Imprimindo saída de debug"
simple_title:         "Imprimindo saída de debug"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

## O Que é & Por Quê

A impressão de output de debug é um método usado por programadores para rastrear o funcionamento do código. Eles fazem isso para encontrar e resolver bugs e melhorar a eficiência do aplicativo.

## Como fazer:

Vou mostrar como o debug é feito no Ruby usando a biblioteca "byebug". Primeiro, precisaremos instalá-la.

```Ruby
gem install byebug
```

Agora, digamos que você tenha o seguinte código e queira fazer debug nele.

```Ruby
def soma(a, b)
  a + b
end

soma(1, 2)
```

Para isso, você adicionaria 'byebug' logo antes da linha que quer inspecionar, como mostrado abaixo:

```Ruby
require 'byebug'

def soma(a, b)
  byebug
  a + b
end

soma(1, 2)
```

Feito isso, ao executar o programa, ele irá parar na linha onde 'byebug' foi chamado, permitindo que você possa inspecionar variáveis e o fluxo do código.

## Mergulho Profundo

Historicamente, a impressão de debug foi uma das primeiras técnicas utilizadas para a depuração de software. Antes de bibliotecas como 'byebug', os programadores costumavam incluir declarações de impressão em partes específicas do código para rastrear o fluxo do programa e o valor das variáveis.

Existem alternativas ao 'byebug', como 'pry'. No entanto, cada uma tem suas próprias vantagens e escolher qual usar depende das necessidades pessoais de cada programador.

Se você está curioso sobre a implementação da impressão de debug, este é essencialmente um processo de I/O - as mensagens de debug são escritas em uma saída (geralmente um console) para serem lidas pelo programador.

## Veja Também

Para mais detalhes e exemplos de como usar o 'byebug', confira a documentação oficial [aqui](https://github.com/deivid-rodriguez/byebug).
Para olhar mais a fundo em alternativas como 'pry', você pode encontrar a documentação [aqui](https://github.com/pry/pry).