---
title:                "Verificando se um diretório existe"
html_title:           "Kotlin: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## O que & Por quê?
Verificar se um diretório existe é o ato de confirmar se uma determinada localização de arquivo está presente ou não. Programadores fazem isso para garantir a integridade de caminhos de arquivos e prevenir erros antes de tentar acessar ou manipular esses diretórios.

## Como fazê-lo:
Aqui está um exemplo usando o FileManager em Swift para verificar a existência de um diretório.

```Swift
import Foundation

let fileManager = FileManager.default

let directoryPath = "/DirectoryPath/You/Are/Checking"

if fileManager.fileExists(atPath: directoryPath) {
    print("O diretório existe.")
} else {
    print("O diretório não existe.")
}
```

Se o diretório especificado existir, você verá `O diretório existe.` Se não, `O diretório não existe.`

## Aprofundamento
No passado, a verificação de diretórios existentes era muito menos eficiente. Antigamente, era comum enumerar todos os diretórios e compará-los à busca. Porém, com a introdução do método `fileExists(atPath:)` no FileManager do Swift, isso se tornou muito mais direto.

Alternativas à essa prática incluem verificar a existência de diretórios por meio de manipulação de erros ao tentar ler ou escrever em um caminho específico.

A implementação `fileExists(atPath:)` vai diretamente ao sistema de arquivos para confirmar se um item existe no caminho especificado. Por isso é importante notar que se o aplicativo não tiver permissão para acessar o caminho especificado, o método retornará `false`.

## Veja Também
Para mais detalhes, consulte a documentação oficial da Apple sobre o [FileManager] (https://developer.apple.com/documentation/foundation/filemanager) e sobre o método/fileExists(atPath:) (https://developer.apple.com/documentation/foundation/filemanager/1410277-fileexists). Além disso, você pode encontrar outras discussões úteis no [Stack Overflow] (https://stackoverflow.com/questions/24097826/read-and-write-a-string-from-text-file).