---
title:    "Swift: Escrevendo para o erro padrão"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que escrever para o erro padrão em programação Swift?

Se você é um programador Swift, provavelmente já se deparou com a função `print()` para imprimir mensagens na tela. Mas, e se você quiser imprimir uma mensagem de erro? É aí que entra a escrita para o erro padrão. Essa técnica permite exibir mensagens de erro de forma mais eficiente e organizada, facilitando a identificação e correção de problemas em seu código.

## Como fazer

Para escrever para o erro padrão em Swift, basta utilizar a função `fwrite()` da biblioteca Foundation. Veja um exemplo de código abaixo:

```
import Foundation

let mensagem = "Erro: o número deve ser positivo."
if let data = mensagem.data(using: .utf8) {
    fwrite(data, 1, data.count, stderr)
}
```

Neste exemplo, definimos uma string contendo a mensagem de erro e a convertemos em dados UTF-8 usando o método `data(using:)`. Em seguida, utilizamos a função `fwrite()` para escrever esses dados no erro padrão usando o ponteiro `stderr`. Ao executar o código, a mensagem será exibida na tela do console, ajudando a identificar e solucionar o erro em questão.

## Mergulho Profundo

Ao escrever para o erro padrão em Swift, é importante compreender a diferença entre os dois ponteiros de saída disponíveis: `stdout` e `stderr`. O primeiro é responsável por exibir mensagens regulares, enquanto o último é usado exclusivamente para mensagens de erro. A utilização desses ponteiros de forma correta facilita a leitura do código e a identificação de possíveis problemas.

## Veja também

- [Documentação do Swift](https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html)
- [Tutorial do Swift: Como escrever para o erro padrão](https://www.raywenderlich.com/5115-error-handling-in-swift)
- [Vídeo explicando a escrita para o erro padrão em Swift](https://www.youtube.com/watch?v=IKZpQW9Cjgo)