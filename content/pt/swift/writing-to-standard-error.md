---
title:                "Escrevendo no erro padrão"
html_title:           "Arduino: Escrevendo no erro padrão"
simple_title:         "Escrevendo no erro padrão"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## O Que é & Porquê?
Escrever no erro padrão é enviar mensagens de erro ou logs para um canal específico, isolando-as da saída padrão do programa. Programadores fazem isso para diagnosticar problemas sem misturar com a saída normal do programa.

## Como Fazer:
```Swift
import Foundation

// Escrevendo uma mensagem no erro padrão
func writeToStandardError(_ message: String) {
    if let messageData = "\(message)\n".data(using: .utf8) {
        FileHandle.standardError.write(messageData)
    }
}

// Uso da função
writeToStandardError("Erro encontrado!")

// Saída esperada no erro padrão:
// Erro encontrado!
```

## Aprofundamento
Antigamente, em sistemas Unix, a saída de erro padrão foi projetada para que mensagens de erro fossem separadas da saída de dados principal. Isso permite a redireção da saída de erros para arquivos de log ou outros destinos para análise. Alternativamente, `stderr` pode ser redirecionado para `stdout` para simplificar a manipulação de saídas. No Swift, `FileHandle.standardError` é a implementação que nos permite escrever diretamente no erro padrão.

## Veja Também
- Documentação oficial da Swift sobre `FileHandle`: [Swift Foundation FileHandle](https://developer.apple.com/documentation/foundation/filehandle)
- Tutorial de manipulação de saídas em Unix: [Unix Redirection](https://www.guru99.com/linux-redirection.html)
