---
title:    "Gleam: Imprimindo saída de depuração"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

#
## Porque
Imprimir saída de depuração é uma técnica muito útil para rastrear problemas em seu código. Isso permite que você visualize o fluxo de execução de seu programa e identifique possíveis erros.

## Como Fazer
Para imprimir a saída de depuração em seu código Gleam, você pode usar a função `debug!()` e passar uma expressão ou variável como argumento. Aqui está um exemplo de como isso pode ser usado:

```Gleam
let my_number = 10
let doubled = debug!(my_number * 2)
```
Este código imprimirá `20` como saída de depuração. Você também pode usar `debug!(some_function())` para imprimir a saída de uma função.

## Mergulho Profundo
A função `debug!()` pode ser útil por mais do que apenas imprimir valores. Você também pode usar a função para imprimir mensagens de texto ou identificadores de código para ajudar a localizar onde a saída de depuração está vindo. Além disso, você pode adicionar chamadas de `debug!()` em diferentes partes do código para ter uma visão mais ampla do fluxo de execução.

## Veja Também
- Documentação oficial do Gleam sobre a função `debug!()` [link](https://gleam.run/documentation/core/function/#debug)
- Artigo sobre depuração de código em Gleam [link](https://medium.com/@gleamlang/advanced-debugging-in-gleam-48d0fe4c074c)