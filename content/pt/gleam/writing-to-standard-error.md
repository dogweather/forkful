---
title:                "Gleam: Escrevendo no erro padrão"
simple_title:         "Escrevendo no erro padrão"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

##Por que escrever para o standard error?

O standard error é uma forma importante de comunicação dentro da programação, permitindo que os desenvolvedores acompanhem e resolvam erros em seus códigos. Ao escrever para o standard error, você pode entender melhor o funcionamento do seu código e detectar problemas de forma mais eficiente.

##Como fazer:

Para escrever para o standard error em Gleam, utilize o comando "stderr.write" seguido da mensagem desejada entre aspas. Veja o exemplo abaixo:

```Gleam
stderr.write("Ocorreu um erro na linha 10!")
```

Isso irá imprimir a mensagem "Ocorreu um erro na linha 10!" no terminal.

##Mais detalhes:

Escrever para o standard error é especialmente útil quando trabalhando com códigos complexos ou em testes. Além de permitir a detecção mais precisa de erros, também ajuda a manter o código organizado e facilita a depuração. No entanto, é importante ter cuidado com a quantidade de mensagens escritas no standard error, já que isso pode sobrecarregar o terminal e dificultar a leitura.

##Veja também:

- Documentação oficial do Gleam sobre o standard error: [link](https://gleam.run/book/stderr.html)
- Artigo sobre melhores práticas na escrita para o standard error: [link](https://www.theserverside.com/blog/Coffee-Talk-Java-News-Stories-and-Opinions/5-Best-Practices-with-Standard-Error-Streams-in-Bash)