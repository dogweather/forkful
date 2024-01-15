---
title:                "Escrevendo para o erro padrão"
html_title:           "Kotlin: Escrevendo para o erro padrão"
simple_title:         "Escrevendo para o erro padrão"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que
Provavelmente você já se deparou com um código que precisa mostrar mensagens de erro ou de depuração ao usuário. Ao invés de exibir essas mensagens no console ou em uma interface gráfica, uma boa prática é escrevê-las diretamente no "standard error" (erro padrão). Isso permite que as mensagens sejam facilmente identificadas e tratadas em situações específicas.

## Como Fazer
Escrever no "standard error" em Kotlin é bastante simples. Basta utilizar a propriedade "err" do objeto System. Por exemplo:

```Kotlin
System.err.println("Erro ao carregar o arquivo!")
```

O código acima irá imprimir a mensagem de erro no "standard error", que pode ser acessado por meio do console ou em outros locais que suportem a exibição de mensagens de erro.

Se você estiver utilizando o framework de teste JUnit, também pode ser útil escrever no "standard error" durante a execução dos testes, para fornecer informações adicionais para debug:

```Kotlin
err.println("Teste falhou na linha ${err.stackTrace[0].lineNumber}.")
```

## Mergulho Profundo
Quando utilizamos a função "println()" no Kotlin, ela escreve a mensagem no "standard output" (saída padrão). Porém, se estivermos lidando com erros, é mais adequado utilizar o "standard error" para escrever as mensagens. Isso pode ser feito utilizando a função "err.println()" como mostrado nos exemplos acima.

Outra diferença importante é que o "standard output" é um fluxo de saída padrão e pode ser redirecionado para outros lugares, como um arquivo de log ou mesmo um outro fluxo de entrada. Já o "standard error" é um fluxo dedicado às mensagens de erro e sempre será exibido no console.

## Veja Também
- [Documentação oficial do Kotlin sobre o objeto System](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-output-stream/)