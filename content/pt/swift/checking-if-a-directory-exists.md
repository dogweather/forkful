---
title:                "Swift: Verificando se um diretório existe"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que verificar se um diretório existe?

Antes de começar a escrever qualquer código em Swift, é importante entender por que certas tarefas são necessárias. Verificar se um diretório existe é importante porque permite que o programa crie novos diretórios ou execute outras ações específicas com base na existência do diretório.

## Como fazer isso:

```Swift 
//Criando uma função para verificar se um diretório existe

func verificarDiretorio(diretorio: String) {
    let fileManager = FileManager.default
    if fileManager.fileExists(atPath: diretorio) {
        print("O diretório existe!")
    } else {
        print("O diretório não existe!")
    }
}
```

Exemplo de saída:
```
verificarDiretorio(diretorio: "/Users/username/Documents")
// Saída: O diretório existe!
    
verificarDiretorio(diretorio: "/Users/username/Desktop/images")
// Saída: O diretório não existe!
```

## Mergulho Profundo:

Para realmente entender como verificamos se um diretório existe em Swift, é importante entender como o processo funciona nos bastidores. Quando chamamos `fileExists(atPath: )` no `FileManager`, o sistema de arquivos é verificado e, em seguida, retorna um `Bool` indicando se o diretório existe ou não.

No entanto, é importante observar que isso não garante que o diretório não será excluído ou movido enquanto o programa estiver sendo executado. É por isso que é sempre importante ter verificações adicionais em seu código para garantir que o diretório esteja presente.

## Veja também:

- [Como criar um diretório usando Swift](https://www.swiftbysundell.com/tips/creating-a-directory-if-it-doesnt-exist/)
- [Documentação oficial do FileManager](https://developer.apple.com/documentation/foundation/filemanager)