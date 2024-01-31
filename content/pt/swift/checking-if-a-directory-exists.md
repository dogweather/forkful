---
title:                "Verificando se um diretório existe"
date:                  2024-01-20T14:58:50.860876-07:00
html_title:           "Fish Shell: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
Verificar a existência de diretórios ajuda a prevenir erros de arquivo não encontrado e permite que programas tomem decisões com base na estrutura atual do sistema de arquivos. Programadores fazem isso para garantir que seus apps leiam ou escrevam só se o diretório estiver lá.

## How to:
Vamos usar a classe `FileManager` para verificar se um diretório existe no Swift.

```Swift
import Foundation

let fileManager = FileManager.default
let directoryPath = "/caminho/para/seu/diretório"

var isDirectory = ObjCBool(false)
if fileManager.fileExists(atPath: directoryPath, isDirectory: &isDirectory) && isDirectory.boolValue {
    print("O diretório existe.")
} else {
    print("O diretório não existe.")
}
```

Saída, dependendo da situação:
```
O diretório existe.
```
ou
```
O diretório não existe.
```

## Deep Dive
Historicamente, a manipulação de arquivos em Swift se apoia fortemente no `FileManager`, que vem do Foundation framework. Alternativamente, você poderia usar APIs de nível mais baixo como `posix` para chegar ao mesmo resultado, mas com complexidade maior.

O método `fileExists(atPath:)` verifica tanto arquivos quanto diretórios. Ao passar um ponteiro booleano `isDirectory`, podemos verificar especificamente por um diretório. Cuidado ao considerar thread-safety: `FileManager.default` é thread-safe, mas instâncias criadas pelo usuário não são garantidas como tal.

Ao trabalhar com iOS, lembre de que o app só pode verificar diretórios dentro de seu sandbox, a menos que você tenha permissões especiais.

## See Also
- Documentação oficial do `FileManager`: https://developer.apple.com/documentation/foundation/filemanager
- Guia sobre o sistema de arquivos do iOS: https://developer.apple.com/library/archive/documentation/FileManagement/Conceptual/FileSystemProgrammingGuide/FileSystemOverview/FileSystemOverview.html
- Tutorial sobre manipulação de arquivos em Swift: https://www.raywenderlich.com/9664901-ios-file-management-with-filemanager
