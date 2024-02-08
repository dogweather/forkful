---
title:                "Verificando se um diretório existe"
aliases:
- pt/swift/checking-if-a-directory-exists.md
date:                  2024-02-03T19:08:39.157233-07:00
model:                 gpt-4-0125-preview
simple_title:         "Verificando se um diretório existe"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Quê?
Verificar se um diretório existe no sistema de arquivos é essencial para gerenciar estruturas de arquivo dentro de suas aplicações Swift. Esta tarefa permite aos desenvolvedores verificar a presença de diretórios antes de tentarem ler ou escrever neles, evitando assim possíveis erros de execução.

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
