---
title:    "Swift: Lendo um arquivo de texto"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Por que

Ler arquivos de texto pode ser uma tarefa muito útil e importante na programação Swift. Ao saber como ler esses arquivos, você poderá criar aplicativos que interajam com dados externos, como dados de um banco de dados ou informações fornecidas por um usuário.

## Como fazer

Para ler um arquivo de texto em Swift, você pode seguir alguns passos simples:

1. Primeiro, declare uma constante ou variável para armazenar o caminho do arquivo de texto. Isso pode ser feito usando o padrão "Bundle.main.path (forResource: "nome do arquivo", ofType: "extensão")", onde "nome do arquivo" representa o nome do arquivo de texto (sem a extensão) e "extensão" representa a extensão do arquivo (por exemplo, "txt" para arquivos de texto).
2. Em seguida, você precisará criar uma instância da classe "FileManager" para gerenciar o arquivo.
3. Use o método "contents(atPath:)" da classe "FileManager" para obter o conteúdo do arquivo no formato de dados "Data".
4. Se necessário, você pode converter os dados em uma string usando o método "String.init(data: encoding:)" e especificando o tipo de codificação.

Uma vez que você tenha lido o arquivo, você pode manipular os dados da maneira que desejar e utilizá-los no seu aplicativo.

Veja um exemplo de código abaixo:

```Swift
if let caminhoArquivo = Bundle.main.path(forResource: "exemplo", ofType: "txt") {
    let gerenciadorArquivo = FileManager()
    if let dados = gerenciadorArquivo.contents(atPath: caminhoArquivo) {
        let texto = String(data: dados, encoding: .utf8)
        print(texto)
    }
}
```

O resultado seria a impressão do conteúdo do arquivo "exemplo.txt" no console.

## Aprofundando

Além do método "contents(atPath:)", existem outras formas de ler arquivos de texto em Swift, como o uso da classe "FileHandle" ou a utilização de bibliotecas de terceiros. Também é importante estar atento à manipulação dos dados lidos para evitar problemas como erros de codificação.

## Veja também

- Documentação oficial da Apple sobre a classe "FileManager" (https://developer.apple.com/documentation/foundation/filemanager)
- Bright Hub Education's blog post sobre como ler arquivos de texto em Swift (https://www.brighthubeducation.com/programming-languages/124098-how-to-open-and-read-a-text-file-in-swift/)
- Biblioteca de terceiros para leitura de arquivos de texto em Swift (https://github.com/Flipboard/FileKit)