---
title:                "C#: Lendo um arquivo de texto."
simple_title:         "Lendo um arquivo de texto."
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que Ler um Arquivo de Texto?

Ler e manipular arquivos de texto é uma habilidade fundamental em programação. Ao aprender como ler arquivos de texto, você estará abrindo um mundo de possibilidades para criar aplicativos robustos e funcionais em C#. Além disso, entender como ler arquivos de texto pode ajudar na automatização de tarefas e no processamento de grandes quantidades de dados.

## Como Ler um Arquivo de Texto em C#

Para ler um arquivo de texto em C#, precisamos seguir alguns passos simples:

1. Abra o arquivo utilizando a classe `StreamReader`.

```C#
StreamReader arquivo = new StreamReader("caminho/do/arquivo.txt);
```

2. Utilize o método `ReadLine()` para ler cada linha do arquivo e atribua o resultado a uma variável.

```C#
string linha = arquivo.ReadLine();
```

3. Utilize um loop para ler todas as linhas do arquivo até que o valor de retorno do método `ReadLine()` seja `null`, indicando o fim do arquivo.

```C#
while (linha != null)
{
    // faça algo com a linha lida
    linha = arquivo.ReadLine();
}
```

4. Não se esqueça de fechar o arquivo após finalizar a leitura, utilizando o método `Close()` da classe `StreamReader`.

```C#
arquivo.Close();
```

## Profundidade na Leitura de Arquivos de Texto

Além dos passos básicos mencionados acima, existem algumas coisas importantes a serem consideradas ao ler arquivos de texto em C#. Primeiramente, é importante ter cuidado com o gerenciamento de memória ao lidar com grandes arquivos. Certifique-se de utilizar o `using` statement e fechar o arquivo adequadamente, para evitar vazamento de memória.

Outro ponto importante é o tratamento de exceções. Ao lidar com arquivos, sempre devemos estar preparados para lidar com possíveis erros, como arquivos ausentes ou permissões insuficientes. Utilize blocos `try-catch` para lidar com essas situações e manter seu código mais robusto.

Além disso, é possível utilizar a classe `File` para realizar operações de leitura e escrita em arquivos de texto sem a necessidade de criar uma instância de `StreamReader`.

## Veja Também

- [Como Escrever em um Arquivo de Texto em C#](https://exemplodeblog.com.br/como-escrever-arquivo-texto-csharp/)
- [Documentação do C#: Manipulando Arquivos de Texto](https://docs.microsoft.com/pt-br/dotnet/csharp/programming-guide/file-system/how-to-manipulate-files-and-directories)
- [Manipulando Arquivos em C#: Leitura e Escrita](https://blog.geekhunter.com.br/manipulando-arquivos-em-c-sharp/)