---
title:    "C#: Escrevendo um arquivo de texto"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Por que escrever um arquivo de texto?

Escrever um arquivo de texto é uma tarefa comum para programadores, pois é uma forma de armazenar informações de maneira organizada e estruturada. Texto é uma forma simples e legível para armazenar dados e informações em um formato fácil de ser lido e manipulado por humanos e máquinas.

## Como fazer:

Para escrever um arquivo de texto em C#, é necessário seguir algumas etapas simples:

1. Criar um objeto do tipo `StreamWriter` e passar o caminho e o nome do arquivo que deseja criar como argumento:
    ```C#
    StreamWriter arquivo = new StreamWriter("meu_arquivo.txt");
    ```
2. Utilize o método `WriteLine` para escrever linhas de texto no arquivo:
    ```C#
    arquivo.WriteLine("Olá, mundo!");
    ```
3. Não se esqueça de fechar o arquivo ao terminar de escrever:
    ```C#
    arquivo.Close();
    ```
4. Pronto! Agora você tem um arquivo de texto contendo "Olá, mundo!".

## Profundidade:

Existem algumas coisas que podem ser úteis para entender melhor como funciona a escrita de um arquivo de texto em C#. Uma delas é usar o argumento `true` no construtor do `StreamWriter`. Isso permite que você adicione linhas ao arquivo sem apagar o conteúdo existente. Por exemplo:

```C#
//Ao invés de criar um novo arquivo, este construtor irá adicionar conteúdo ao existente
StreamWriter arquivo = new StreamWriter("meu_arquivo.txt", true);

arquivo.WriteLine("Uma nova linha adicionada ao final do arquivo.");

arquivo.Close();
```

Outra coisa a se ter em mente é o uso do `using` statement, que garante que o arquivo será fechado automaticamente ao final do bloco do código. Por exemplo:

```C#
using (StreamWriter arquivo = new StreamWriter("meu_arquivo.txt")) 
{
    arquivo.WriteLine("Este texto será automaticamente adicionado ao arquivo.");

    //O arquivo será fechado automaticamente ao sair deste bloco
}
```

## Veja também:

- [Documentação oficial do C# sobre `StreamWriter`](https://docs.microsoft.com/pt-br/dotnet/api/system.io.streamwriter?view=netcore-3.1)