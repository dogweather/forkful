---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:39.157233-07:00
description: "Como fazer: O framework Foundation do Swift fornece a classe `FileManager`,\
  \ que possui m\xE9todos para gerenciar o sistema de arquivos. Voc\xEA pode usar\
  \ o\u2026"
lastmod: '2024-03-13T22:44:46.935197-06:00'
model: gpt-4-0125-preview
summary: "O framework Foundation do Swift fornece a classe `FileManager`, que possui\
  \ m\xE9todos para gerenciar o sistema de arquivos."
title: "Verificando se um diret\xF3rio existe"
weight: 20
---

## Como fazer:
O framework Foundation do Swift fornece a classe `FileManager`, que possui métodos para gerenciar o sistema de arquivos. Você pode usar o `FileManager` para verificar se um diretório existe. Aqui está um trecho sobre como fazer isso:

```swift
import Foundation

let fileManager = FileManager.default
let path = "/caminho/para/seu/diretório"

if fileManager.fileExists(atPath: path, isDirectory: nil) {
    print("O Diretório existe")
} else {
    print("O Diretório não existe")
}
```

No entanto, isso verifica tanto arquivos quanto diretórios. Se você quiser verificar especificamente se um diretório existe, você precisa passar um ponteiro para um valor Booleano em `isDirectory`:

```swift
import Foundation

let fileManager = FileManager.default
let path = "/caminho/para/seu/diretório"
var isDirectory: ObjCBool = false

if fileManager.fileExists(atPath: path, isDirectory: &isDirectory), isDirectory.boolValue {
    print("O Diretório existe")
} else {
    print("O Diretório não existe")
}
```

### Usando uma Biblioteca de Terceiros
Até o momento, verificar a existência de um diretório em Swift geralmente não necessita de bibliotecas de terceiros devido à robustez da classe `FileManager`. No entanto, para manipulações e verificações de arquivos mais complexas, bibliotecas como **Files** de John Sundell oferecem uma API mais amigável ao Swift.

Aqui está como você pode usá-la:

Primeiramente, adicione Files ao seu projeto via Swift Package Manager.

Então, você pode verificar a existência de um diretório assim:

```swift
import Files

do {
    _ = try Folder(path: "/caminho/para/seu/diretório")
    print("O Diretório existe")
} catch {
    print("O Diretório não existe")
}
```

Nota: Como bibliotecas de terceiros podem mudar, sempre consulte a documentação mais recente para uso e melhores práticas.
