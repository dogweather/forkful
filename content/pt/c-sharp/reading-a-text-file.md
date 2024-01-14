---
title:    "C#: Lendo um arquivo de texto"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que

Muitas vezes, em programação, precisamos ler e manipular dados armazenados em arquivos de texto. Esses arquivos podem conter informações importantes para o funcionamento de um programa ou até mesmo dados de usuário. Portanto, é essencial saber como ler um arquivo de texto em C#.

## Como Fazer

Para ler um arquivo de texto em C#, primeiro precisamos abrir o arquivo usando a classe `StreamReader`. Em seguida, podemos utilizar o método `ReadLine()` para ler uma linha de cada vez e armazená-la em uma variável. É importante usar um laço de repetição (`while` ou `foreach`) para percorrer todas as linhas do arquivo. Abaixo segue um exemplo de código:

```C#
//abrindo o arquivo
StreamReader arquivo = new StreamReader("arquivo.txt");

//variável para armazenar cada linha lida
string linha;

//percorrendo todas as linhas do arquivo
while ((linha = arquivo.ReadLine()) != null)
{
    Console.WriteLine(linha); //imprimindo a linha lida na tela
}

//fechando o arquivo
arquivo.Close();
```

A saída deste código, se o arquivo tiver o conteúdo "Olá, mundo!", será:

```
Olá, mundo!
```

## Mergulho Profundo

Para uma leitura mais eficiente e precisa de arquivos de texto, podemos utilizar a classe `File` do C#. Essa classe possui diversos métodos úteis para trabalhar com arquivos, como `ReadAllText()` para ler todo o conteúdo do arquivo de uma vez, `AppendAllText()` para adicionar texto ao final do arquivo e muito mais.

Além disso, podemos utilizar os métodos `TextReader` e `TextWriter` para ler e escrever em arquivos de texto de forma mais rápida e eficiente.

## Veja Também

- [Documentação oficial do StreamReader](https://docs.microsoft.com/pt-br/dotnet/api/system.io.streamreader)
- [Documentação oficial do File](https://docs.microsoft.com/pt-br/dotnet/api/system.io.file)
- [Tutorial completo sobre leitura e escrita de arquivos em C#](https://www.devmedia.com.br/trabalhando-com-arquivos-em-csharp/23635)