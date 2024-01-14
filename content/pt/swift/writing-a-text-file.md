---
title:                "Swift: Escrevendo um arquivo de texto"
simple_title:         "Escrevendo um arquivo de texto"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Por que escrever um arquivo de texto no Swift?

Escrever um arquivo de texto no Swift pode ser útil para armazenar informações importantes em um formato de fácil leitura e manipulação. Além disso, arquivos de texto podem ser lidos e modificados por diversos softwares e linguagens, tornando-o uma opção versátil para armazenar dados.

## Como fazer:

Para iniciar, crie uma instância da classe `FileManager` para gerenciar o sistema de arquivos. Em seguida, utilize o método `createFile()` para criar um novo arquivo de texto. Para escrever no arquivo, utilize o método `write()` passando o conteúdo desejado como parâmetro.

```Swift
let fileManager = FileManager.default //cria instância do FileManager
let fileURL = fileManager.urls(for: .documentDirectory, in: .userDomainMask)[0].appendingPathComponent("meu_arquivo.txt") //gera URL para o novo arquivo
let texto = "Este é um exemplo de texto que será escrito em um arquivo de texto." //conteúdo a ser escrito no arquivo

do {
    try fileManager.createFile(atPath: fileURL.path, contents: nil) //cria o arquivo
    try texto.write(to: fileURL, atomically: false, encoding: .utf8) //escreve o conteúdo no arquivo
} catch {
    print("Erro ao criar ou escrever no arquivo.")
}
```

Para verificar se o arquivo foi criado e se o conteúdo foi escrito corretamente, você pode ler o arquivo e imprimir seu conteúdo:

```Swift
do {
    let arquivo = try String(contentsOf: fileURL, encoding: .utf8) //lê o conteúdo do arquivo
    print(arquivo) //imprime o conteúdo na tela
} catch {
    print("Erro ao ler o arquivo.")
}
```

O output impresso será "Este é um exemplo de texto que será escrito em um arquivo de texto."

## Deep Dive:

Ao escrever um arquivo de texto, é importante entender que existem diversos formatos que podem ser utilizados, como por exemplo o formato CSV ou o formato JSON. Cada formato possui suas próprias regras e convenções, e é importante pesquisar e escolher o mais adequado para o seu caso de uso.

Além disso, é importante garantir que o texto escrito esteja devidamente formatado e estruturado, a fim de facilitar a leitura e a manipulação. Por exemplo, utilizar quebras de linha `\n` para separar linhas de texto ou utilizar tabs `\t` para indentação.

# Veja também:

- [Documentação oficial do Swift FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [Tutorial de escrita de arquivos no Swift](https://www.hackingwithswift.com/example-code/strings/how-to-write-to-a-text-file)
- [Diferentes formatos de arquivo de texto](https://en.wikipedia.org/wiki/Plain_text)