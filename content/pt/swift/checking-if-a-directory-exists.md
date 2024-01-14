---
title:                "Swift: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

##Por que verificar se um diretório existe?

A verificação da existência de um diretório é uma tarefa importante em programação, pois permite que seu código tome decisões baseadas na presença ou ausência de um diretório específico. Isso pode ser útil em várias situações, como garantir que um arquivo seja salvo em um local específico ou verificar se um diretório necessário para sua aplicação está presente antes de continuar com a execução do código.

##Como fazer?

Para verificar se um diretório existe em Swift, podemos usar o método `FileManager.default.fileExists(atPath:)`. Este método recebe um caminho de string como parâmetro e retorna um valor booleano, indicando se o diretório existe ou não. Por exemplo:

```Swift
let fileManager = FileManager.default
let documentsDirectoryPath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] as String
let directoryPath = documentsDirectoryPath + "/NovoDiretorio"

if fileManager.fileExists(atPath: directoryPath) {
    print("O diretório já existe")
} else {
    print("O diretório não existe")
}
```

Neste exemplo, estamos verificando se o diretório "NovoDiretorio" existe dentro do diretório de documentos do usuário. Se o diretório existir, a mensagem "O diretório já existe" será impressa, caso contrário, a mensagem "O diretório não existe" será exibida.

##Aprofundando-se

Além de verificar se um diretório existe, também é possível criar um novo diretório com o método `FileManager.default.createDirectory(atPath:)`. Este método também recebe um caminho de string como parâmetro e retorna um valor booleano, indicando se a criação foi bem sucedida ou não.

```Swift
let fileManager = FileManager.default
let documentsDirectoryPath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] as String
let newDirectoryPath = documentsDirectoryPath + "/NovoDiretorio"

do {
    try fileManager.createDirectory(atPath: newDirectoryPath, withIntermediateDirectories: true, attributes: nil)
    print("Diretório criado com sucesso")
} catch let error as NSError {
    print("Erro ao criar o diretório: \(error)")
}
```

Neste exemplo, estamos criando um novo diretório chamado "NovoDiretorio" dentro do diretório de documentos do usuário. É importante especificar o parâmetro `withIntermediateDirectories` como `true`, pois isso garantirá que o método crie todos os diretórios intermediários necessários para criar o diretório final.

##Veja também

- [Documentação do método fileExists](https://developer.apple.com/documentation/foundation/filemanager/2293546-fileexists)
- [Documentação do método createDirectory](https://developer.apple.com/documentation/foundation/filemanager/1410909-createdirectory)

Esperamos que este artigo tenha sido útil para você entender como verificar a existência de um diretório em Swift. Fique à vontade para explorar mais sobre esses métodos e aprimorar suas habilidades de programação. Até a próxima!