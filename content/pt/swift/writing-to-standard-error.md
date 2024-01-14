---
title:                "Swift: Escrevendo para o erro padrão"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que escrever no erro padrão?

Escrever no erro padrão, também conhecido como standard error, é um processo importante para a depuração e rastreamento de erros em um programa Swift. Ao imprimir mensagens de erro no erro padrão, você pode identificar e corrigir problemas no código com mais facilidade.

## Como fazer isso?

O Swift possui uma função integrada para escrever no erro padrão, chamada `print(_:to:)`. Esta função aceita dois argumentos: a mensagem de erro a ser impressa e a saída desejada, que no nosso caso é o erro padrão. O código a seguir mostra como usá-la:

```Swift
let errorMessage = "Erro na linha 23: variável não inicializada."
print(errorMessage, to: &standardError)
```

Ao rodar esse código, a mensagem de erro será impressa no console ou terminal, dependendo de onde você estiver executando seu programa. O output será algo parecido com:

```bash
Erro na linha 23: variável não inicializada.
```

## Mergulho profundo

Além da função `print(_:to:)`, o Swift também possui a estrutura `FileHandle`, que fornece uma maneira mais avançada de escrever no erro padrão. Esta estrutura possui métodos específicos para lidar com a saída de erro, como `write(_:Data)`, que aceita uma string ou array de bytes como argumento para ser escrito no erro padrão. Você também pode usar o método `seek(toOffset:)` para especificar um local específico na saída do erro e sobrescrever o que foi escrito anteriormente.

Vale ressaltar que a saída do erro padrão pode ser redirecionada para um arquivo ou outro dispositivo, se necessário. Isso pode ser feito usando a função `dup2(_:Int32,_:Int32)`. No entanto, este é um assunto mais avançado e não será abordado neste artigo.

## Veja também

Para aprender mais sobre como trabalhar com o erro padrão em Swift, você pode consultar os seguintes recursos:

- [Documentação oficial do Swift sobre o objeto `FileHandle`](https://developer.apple.com/documentation/foundation/filehandle)
- [Tutorial sobre saída de erro em Swift](https://theswiftdev.com/mastering-stdout-stderr-in-swift/)
- [Outra abordagem para escrever no erro padrão em Swift](https://www.swiftbysundell.com/articles/standard-output-in-swift/)