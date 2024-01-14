---
title:                "Gleam: Escrevendo para o erro padrão"
programming_language: "Gleam"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que escrever para o erro padrão?

Escrever para o erro padrão é uma técnica útil para lidar com erros e exceções no seu código Gleam. Ela permite que você identifique com mais facilidade quaisquer problemas que possam surgir durante a execução do seu programa.

## Como fazer

Para escrever para o erro padrão em seu código Gleam, basta usar a função `io.write_error/1` e passar como argumento uma string contendo a mensagem de erro que deseja exibir. Por exemplo:

```
Gleam
def escrever_erro() {
  io.write_error("Erro: Não é possível dividir por zero")
}
```

Este código irá escrever a mensagem de erro "Erro: Não é possível dividir por zero" no terminal ao ser executado.

## Aprofundando-se

Existem algumas coisas importantes a serem consideradas ao escrever para o erro padrão em seu código Gleam. Primeiro, é importante usar as informações disponíveis para fornecer uma mensagem de erro clara e concisa. Isso pode incluir informações sobre qual parte do código causou o erro ou como o usuário pode corrigi-lo.

Além disso, é importante lembrar de sempre lidar com as possíveis exceções que possam surgir ao escrever para o erro padrão. Isso pode incluir o uso de try/catch blocks para lidar com erros específicos ou garantir que todas as exceções sejam capturadas.

## Veja também

- [Documentação oficial do Gleam sobre escrever para o erro padrão](https://gleam.run/documentation/error_handling/#writing-to-standard-error)
- [Um tutorial do Gleam sobre tratamento de erros](https://gleam.run/documentation/tutorials/error_handling.html)
- [Um artigo sobre as melhores práticas de tratamento de erros no Gleam](https://medium.com/@gleamlang/best-practices-for-error-handling-in-gleam-986329ea75e)