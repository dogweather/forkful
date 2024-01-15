---
title:                "Escrevendo um arquivo de texto"
html_title:           "C#: Escrevendo um arquivo de texto"
simple_title:         "Escrevendo um arquivo de texto"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por que

Escrever um arquivo de texto, apesar de parecer uma tarefa simples, pode ser muito importante para quem programa. Isso porque os arquivos de texto são uma forma de persistir dados importantes e compartilhá-los com outros usuários ou sistemas.

## Como fazer

Para escrever um arquivo de texto em C#, é necessário seguir alguns passos básicos:

- Primeiro, devemos criar um objeto do tipo `StreamWriter` que será responsável por escrever no arquivo. Para isso, utilizamos o comando `new` seguido do nome da classe e o nome desejado para o objeto.

```C#
StreamWriter file = new StreamWriter("meu_arquivo.txt");
```

- Em seguida, utilizamos o método `WriteLine()` para escrever uma linha no arquivo. Podemos repetir esse comando quantas vezes forem necessárias para escrever todo o conteúdo desejado.

```C#
file.WriteLine("Este é um exemplo de linha escrita em um arquivo de texto.");
file.WriteLine("Podemos adicionar quantas linhas quisermos para criar um arquivo completo.");
```

- Por fim, é importante fechar o arquivo utilizando o método `Close()` para garantir que todas as alterações sejam salvas.

```C#
file.Close();
```

O resultado será um arquivo chamado "meu_arquivo.txt" com o seguinte conteúdo:

```
Este é um exemplo de linha escrita em um arquivo de texto.
Podemos adicionar quantas linhas quisermos para criar um arquivo completo.
```

## Aprofundando

Além de escrever linhas de texto, é possível realizar outras operações em arquivos de texto utilizando a classe `StreamWriter`. Por exemplo, podemos utilizar o método `Write()` para escrever um texto sem quebrar linha, ou o método `Flush()` para forçar o conteúdo a ser gravado no arquivo.

Outra alternativa é utilizar o construtor da classe `StreamWriter` para especificar o formato de codificação desejado para o arquivo de texto. Por padrão, o formato UTF-8 é utilizado, mas é possível escolher outros como UTF-16, ASCII, entre outros.

## Veja também

- [Documentação oficial da classe StreamWriter em C#](https://docs.microsoft.com/pt-br/dotnet/api/system.io.streamwriter)
- [Tutorial de escrita em arquivos de texto com C#](https://www.devmedia.com.br/lendo-e-escrevendo-arquivos-de-texto-com-csharp/26745)