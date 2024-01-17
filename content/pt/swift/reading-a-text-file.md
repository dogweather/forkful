---
title:                "Lendo um arquivo de texto"
html_title:           "Swift: Lendo um arquivo de texto"
simple_title:         "Lendo um arquivo de texto"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

# O que e Porque?
Ler um arquivo de texto é o ato de acessar e interpretar o conteúdo escrito em um arquivo de formato simples como .txt ou .csv. Os programadores muitas vezes precisam ler arquivos de texto para obter informações úteis e usá-las em seus programas.

# Como fazer:
Para ler um arquivo de texto em Swift, primeiro é necessário criar uma instância de FileManager, que permite o acesso aos arquivos do sistema. Em seguida, é preciso especificar o caminho do arquivo que deseja ser lido. Por fim, basta utilizar o método String(contentsOfFile:) para obter o conteúdo do arquivo e trabalhar com ele da maneira que desejar.

```Swift
let fileManager = FileManager()
let filePath = "CaminhoDoArquivo.txt"
if let contents = try? String(contentsOfFile: filePath) {
    // Manipule o conteúdo do arquivo aqui
}
```

# Profundidade:
Ler arquivos de texto é uma prática comum entre os programadores, pois permite a leitura e utilização de dados essenciais para seus programas. Antigamente, os arquivos de texto eram a única forma de armazenar informações em computadores, mas hoje existem alternativas como bancos de dados. A leitura de um arquivo de texto também pode ser feita de forma assíncrona, tornando o processo mais eficiente.

# Veja também:
Para mais informações sobre a leitura de arquivos em Swift, consulte a documentação oficial da Apple: https://developer.apple.com/documentation/foundation/filemanager.