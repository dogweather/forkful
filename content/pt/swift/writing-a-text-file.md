---
title:                "Swift: Escrever um arquivo de texto"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/writing-a-text-file.md"
---

{{< edit_this_page >}}

##Por que Escrever um Arquivo de Texto

Existem muitas razões pelas quais alguém pode querer escrever um arquivo de texto usando Swift. Talvez você queira armazenar dados em um formato fácil de ler e editar, ou talvez queira gravar um log de informações de seu aplicativo. Seja qual for o motivo, escrever um arquivo de texto pode ser uma habilidade valiosa a se ter na programação Swift.

##Como Escrever um Arquivo de Texto

Para escrever um arquivo de texto em Swift, você pode seguir estes passos simples:

1. Criar uma instância da classe `FileManager` para gerenciar o processo de gravação do arquivo.
2. Definir o conteúdo que você deseja escrever no arquivo.
3. Escolher o local em que deseja salvar o arquivo. Isso pode ser feito usando o diretório de documentos do usuário ou criando um novo diretório.
4. Criar um objeto `FileHandle` para abrir e escrever no arquivo.
5. Usar o método `write(_:)` do objeto `FileHandle` para gravar o conteúdo no arquivo.
6. Encerrar o arquivo usando o método `closeFile()`.

Veja um exemplo de código abaixo:

```Swift
let fileManager = FileManager.default
let content = "Olá mundo! Este é um arquivo de texto escrito em Swift."
if let documentsDirectory = fileManager.urls(for: .documentDirectory, in: .userDomainMask).first {
    let fileURL = documentsDirectory.appendingPathComponent("meuTexto.txt")
    do {
        try content.write(to: fileURL, atomically: false, encoding: .utf8)
        print("Arquivo de texto salvo com sucesso em: \(fileURL)")
    } catch {
        print("Erro ao salvar arquivo: \(error)")
    }
}
```

O arquivo de texto será salvo no diretório de documentos do usuário com o nome "meuTexto.txt" e o conteúdo será o texto definido na constante `content`.

##Aprofundando-se na Escrita de Arquivos de Texto

Se você quiser saber mais sobre a escrita de arquivos de texto em Swift, pode explorar conceitos como codificação e decodificação de dados, manipulação de erros e gerenciamento de diretórios. Além disso, você pode explorar diferentes formas de escrever no arquivo, como adicionar conteúdo ao final do arquivo ou substituir todo o seu conteúdo.

Uma boa prática ao trabalhar com arquivos de texto é sempre verificar se o arquivo foi criado corretamente e se o conteúdo foi gravado com sucesso nele. Isso pode ser feito usando o método `fileExists(atPath:)` da classe `FileManager` e verificando se ele retorna `true`, bem como lidar com possíveis erros ao escrever no arquivo.

##Veja também
- [Documentação oficial da classe FileManager no Swift](https://developer.apple.com/documentation/foundation/filemanager)
- [Guia de aprendizado de Swift da Apple: Trabalhando com Arquivos e Diretórios](https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/FilesAndDirectories.html)
- [Artigo do Hacking With Swift: Como escrever no sistema de arquivos do iOS usando Swift](https://www.hackingwithswift.com/example-code/system/how-to-write-to-the-ios-file-system-using-swift)