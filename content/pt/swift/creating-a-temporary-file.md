---
title:                "Criando um arquivo temporário"
date:                  2024-01-20T17:41:39.139174-07:00
model:                 gpt-4-1106-preview
simple_title:         "Criando um arquivo temporário"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why?
Criar um arquivo temporário é o processo de gerar um arquivo que é usado durante a execução de um programa, mas que não é necessário após o fim do uso. Programadores fazem isso para armazenar dados transitórios, como caching, evitar conflitos de escrita ou manter informações sensíveis que não devem persistir em um armazenamento mais durável.

## How to:
Swift facilita a criação de arquivos temporários usando o pacote `Foundation`. Aqui está um exemplo rápido:

```Swift
import Foundation

let temporaryDirectoryURL = FileManager.default.temporaryDirectory
let temporaryFilename = ProcessInfo.processInfo.globallyUniqueString
let temporaryFileURL = temporaryDirectoryURL.appendingPathComponent(temporaryFilename)

do {
    try "Dados temporários".write(to: temporaryFileURL, atomically: true, encoding: .utf8)
    print("Arquivo temporário criado em: \(temporaryFileURL.path)")
} catch {
    print(error)
}

// Não esqueça de deletar o arquivo quando terminar!
do {
    try FileManager.default.removeItem(at: temporaryFileURL)
    print("Arquivo temporário deletado.")
} catch {
    print(error)
}
```

Saída esperada:

```
Arquivo temporário criado em: /path/to/temporary/directory/unique-temporary-filename
Arquivo temporário deletado.
```

## Deep Dive:
Desde os tempos do Unix, arquivos temporários são essenciais para programas que precisam manipular dados de maneira isolada. Em Swift, a classe `FileManager` gerencia a criação e remoção desses arquivos. Uma alternativa ao método mostrado seria utilizar a função `mkstemp()` da biblioteca padrão C para maior controle, mas isso exige lidar com APIs mais baixo nível e não é tão Swifty.

A geração de um nome único para o arquivo temporário é crucial para evitar colisões e potenciais vulnerabilidades de segurança. A classe `ProcessInfo` oferece uma string única globalmente que serve bem a esse propósito.

Por fim, lembre-se de que os arquivos temporários criados devem ser limpos após o uso para evitar o desperdício de recursos. Alguns sistemas limpam automaticamente a pasta temporária, mas é uma boa prática de desenvolvimento você mesmo fazer a limpeza.

## See Also:
- Documentação do Swift sobre `FileManager`: https://developer.apple.com/documentation/foundation/filemanager
- Guia da Apple sobre sistema de arquivos: https://developer.apple.com/library/archive/documentation/FileManagement/Conceptual/FileSystemProgrammingGuide/Introduction/Introduction.html
- Artigo sobre segurança e arquivos temporários: https://www.owasp.org/index.php/Insecure_Temporary_File
- Informação sobre `mkstemp()` em C: https://man7.org/linux/man-pages/man3/mkstemp.3.html