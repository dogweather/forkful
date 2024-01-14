---
title:                "C#: Lendo um arquivo de texto"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Porquê

Ler e manipular arquivos de texto é uma habilidade essencial para qualquer programador em C#. Além de permitir a leitura de grandes quantidades de dados, essa técnica também é de extrema importância para a interação com usuários, salvando e recuperando informações importantes. Neste post, vamos explorar como ler um arquivo de texto usando C# e como essa habilidade pode ser útil em diversos projetos de programação.

## Como Fazer

### Exemplo 1: Lendo um arquivo de texto

Primeiramente, precisamos criar uma instância da classe `StreamReader` que será responsável por ler o arquivo de texto. Para isso, utilizamos o construtor da classe que recebe como parâmetro o diretório do arquivo:

```C#
StreamReader reader = new StreamReader("Caminho/Do/Arquivo/Texto.txt");
```

Em seguida, usamos o método `ReadLine()` para ler uma linha do arquivo e armazená-la em uma variável. Esse processo pode ser feito dentro de um loop para ler todas as linhas do arquivo:

```C#
string line;
while ((line = reader.ReadLine()) != null)
{
    Console.WriteLine(line);
}
```

Por fim, não podemos esquecer de fechar o arquivo após a leitura, utilizando o método `Close()`:

```C#
reader.Close();
```

### Exemplo 2: Salvando informações em um arquivo de texto

Além de ler informações de um arquivo de texto, podemos também escrever e salvar informações para uso posterior. Para isso, utilizamos a classe `StreamWriter` da mesma forma que a classe `StreamReader`, porém com um parâmetro adicional que determina o modo de escrita do arquivo:

```C#
       StreamWriter writer = new StreamWriter("Caminho/Do/Arquivo/Texto.txt", true);
```

O segundo parâmetro (`true`) indica que o arquivo será criado caso não exista, ou terá novas informações adicionadas ao final caso já exista. Para escrever no arquivo, utilizamos o método `Write()` ou `WriteLine()`, da seguinte forma:

```C#
writer.WriteLine("Nova linha adicionada");
```

E como sempre, devemos fechar o arquivo após a escrita utilizando o método `Close()`:

```C#
writer.Close();
```

## Aprofundando-se

Além dos métodos citados acima, existem outras formas de ler e manipular arquivos de texto em C#. A classe `File` possui métodos estáticos para realizar operações com arquivos de texto, como `ReadAllText()` e `WriteAllText()`. Além disso, podemos usar a classe `FileInfo` para obter informações como tamanho e data de criação do arquivo.

Outro ponto importante é que os arquivos de texto também podem ser lidos e escritos em diferentes formatos, como UTF-8, UTF-16 e ASCII. Para isso, basta utilizar as classes `Encoding` e `Decoding` para converter os dados.

## Veja Também

- [Documentação oficial da Microsoft sobre a classe StreamReader](https://docs.microsoft.com/pt-br/dotnet/api/system.io.streamreader?view=net-5.0)
- [Tutorial sobre leitura de arquivos de texto em C#](https://www.devmedia.com.br/trabalhando-com-arquivos-de-texto-em-csharp/21426)
- [Consumindo e escrevendo arquivos de texto em diferentes formatos](https://pt.stackoverflow.com/questions/245876/como-l%c3%aar-e-gravar-um-arquivo-txt-com-unicode-em-c-sharp)