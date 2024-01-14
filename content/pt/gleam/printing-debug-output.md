---
title:                "Gleam: Imprimindo saída de depuração."
programming_language: "Gleam"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que

Você já se deparou com um bug no seu programa e passou horas tentando descobrir onde estava o problema? Ou talvez você simplesmente queira entender melhor o fluxo do seu código enquanto está desenvolvendo? Seja qual for o motivo, imprimir saídas de depuração é uma técnica útil para entender o comportamento do seu código e identificar possíveis erros.

## Como fazer

A impressão de saídas de depuração no Gleam é bastante simples. Basta usar a função `debug!` com uma expressão ou valor como argumento dentro de um bloco de código `Gleam`:

```
Gleam> debug!(minha_variavel)
```

Isso imprimirá o valor da variável no console, permitindo que você visualize seu valor atual durante a execução do código. Você também pode imprimir várias variáveis de uma vez, separando-as por vírgula:

```
Gleam> debug!(primeira_variavel, segunda_variavel)
```

Além disso, você também pode imprimir mensagens adicionais para facilitar a compreensão da saída:

```
Gleam> debug!("O valor da primeira variável é", primeira_variavel)
```

## Profundidade

Além de simplesmente imprimir valores e mensagens, a função `debug!` também permite que você acesse informações mais detalhadas sobre as variáveis e o estado do programa. Por exemplo, você pode imprimir o tipo de uma variável usando a função `type_of`:

```
Gleam> debug!(type_of(minha_variavel))
```

Você também pode imprimir o valor de uma variável em um determinado ponto de um padrão de combinação. Por exemplo:

```
Gleam> debug!(match minha_variavel {
  True -> "A variável é verdadeira"
  False -> "A variável é falsa"
})
```

A impressão de saídas de depuração é uma técnica poderosa que pode ser usada para melhorar seu processo de desenvolvimento e tornar a depuração de erros mais eficiente.

## Veja também

- Documentação oficial do Gleam sobre saídas de depuração: https://gleam.run/documentation/guides/debug
- Vídeo do Devs com Gleam: Depuração: https://www.youtube.com/watch?v=rJ11LkAKyE0