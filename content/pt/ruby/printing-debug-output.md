---
title:                "Imprimindo saída de depuração"
html_title:           "Ruby: Imprimindo saída de depuração"
simple_title:         "Imprimindo saída de depuração"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

## O que & Por quê?
Imprimir a saída de depuração é uma técnica usada pelos programadores para acompanhar seu código e identificar problemas durante o processo de desenvolvimento. Isso pode incluir a visualização de valores de variáveis, mensagens de erro e outras informações úteis para encontrar e solucionar erros no código.

## Como Fazer:
Para imprimir uma saída de depuração em seu código Ruby, você pode usar o método `p` ou `puts`. Aqui está um exemplo simples:

```
num = 5
puts "O número é: #{num}"
```

Isso irá imprimir a seguinte saída:

```
O número é: 5
```

Se você quiser imprimir uma mensagem de erro, pode fazer o seguinte:

```
puts "Oops, algo deu errado!"
```

E isso irá imprimir a seguinte saída:

```
Oops, algo deu errado!
```

## Mergulho Profundo:
A técnica de imprimir a saída de depuração é frequentemente usada como uma forma rápida e fácil de rastrear e corrigir erros no código. No entanto, existem outras técnicas que podem ser usadas para depurar, como o uso de ferramentas de rastreamento e o uso de testes automatizados. Além disso, imprimir a saída de depuração pode afetar o desempenho do seu código, pois pode exigir que o programa pare a execução para imprimir os valores. É importante encontrar um equilíbrio entre o uso dessa técnica e outras técnicas de depuração.

## Veja Também:
Se você quiser saber mais sobre depuração em Ruby, aqui estão algumas fontes úteis:
- [Guia de Depuração Oficial do Ruby](https://ruby-doc.org/core-2.7.1/Debugger.html)
- [Vídeo explicando como depurar em Ruby](https://www.youtube.com/watch?v=Zs_DYl-5jkc)
- [Ferramenta de rastreamento para Ruby chamada Pry](https://github.com/pry/pry)