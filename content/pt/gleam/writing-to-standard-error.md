---
title:    "Gleam: Escrevendo para o erro padrão"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Por que escrever para o erro padrão

Escrever para o erro padrão é uma técnica muito útil para programadores que desejam melhorar a qualidade do seu código. Ao imprimir mensagens de erro ou avisos no erro padrão, é possível depurar o código de forma mais eficiente e entender melhor o funcionamento do programa.

## Como fazer

Para escrever para o erro padrão em Gleam, basta utilizar a função `io.fwrite` passando como primeiro argumento o identificador do erro padrão (`error`) e, em seguida, a mensagem que deseja imprimir entre as aspas duplas.

```Gleam
io.fwrite(error, "Essa é uma mensagem de erro")
```

Caso deseje formatar a mensagem de forma mais elaborada, é possível utilizar a função `fmt` e passar o resultado como segundo argumento para `io.fwrite`.

```Gleam
let mensagem = fmt("O programa parou na linha {} com o erro '{}'", [line, error])
io.fwrite(error, mensagem)
```

## Profundando-se

Existem diferentes motivos para escrever para o erro padrão em Gleam. Além de auxiliar no processo de depuração, essa técnica também pode ser útil para escrever logs e monitorar o desempenho do programa.

Uma boa prática é sempre utilizar mensagens claras e informativas ao escrever para o erro padrão, para que seja mais fácil entender o código no futuro ou para que outros programadores possam entender e solucionar problemas no seu código.

## Veja também

- Documentação oficial da função `io.fwrite`: [https://gleam.run/modules/io#fwrite](https://gleam.run/modules/io#fwrite)
- Tutorial sobre como depurar código em Gleam: [https://gleam.run/tutorials/debugging](https://gleam.run/tutorials/debugging)
- Artigo sobre a importância de escrever mensagens de erro claras: [https://medium.com/digitalindia/logging-errors-in-your-python-application-94d243f78baa](https://medium.com/digitalindia/logging-errors-in-your-python-application-94d243f78baa)