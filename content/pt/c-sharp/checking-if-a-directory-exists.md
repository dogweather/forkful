---
title:    "C#: Verificando se um diretório existe"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Porque

Existem várias razões pelas quais um programador pode querer verificar se um diretório existe em seu código, como garantir que um determinado arquivo possa ser acessado ou criado corretamente. Saber como verificar isso no seu programa pode ser útil em diversas situações.

## Como fazer

Aqui estão alguns exemplos de código em C# que mostram como verificar se um diretório existe em diferentes cenários:

```C#
// Verifica se um diretório existe
if(Directory.Exists("C:/Users/Usuario/Documentos/MeuDiretorio")){
    Console.WriteLine("O diretório existe!");
}

// Verifica e cria um diretório caso não exista
string caminho = "C:/Users/Usuario/Documentos/NovoDiretorio";
if(!Directory.Exists(caminho)){
    Directory.CreateDirectory(caminho);
    Console.WriteLine("Diretório criado: " + caminho);
}

// Verifica e deleta um diretório caso exista
string caminho2 = "C:/Users/Usuario/Documentos/DiretorioDeletado";
if(Directory.Exists(caminho2)){
    Directory.Delete(caminho2);
    Console.WriteLine("Diretório deletado: " + caminho2);
}
```

Observe que a classe `Directory` é a responsável por fornecer os métodos necessários para verificar e manipular diretórios em C#. Além disso, é importante lembrar de utilizar o caminho completo do diretório, incluindo a unidade de disco, para evitar erros.

## Mais informações

A verificação de diretórios vai além de apenas verificar sua existência. Existem também opções para checar se um diretório é um diretório de sistema ou se possui permissões de acesso específicas. Além disso, a classe `Directory` também possui métodos para trabalhar com subdiretórios e listar arquivos dentro de um diretório. Para saber mais sobre todas as possibilidades de verificação e manipulação de diretórios em C#, consulte a documentação oficial da classe `Directory`.

## Veja também

- [Documentação oficial da classe Directory em C#](https://docs.microsoft.com/pt-br/dotnet/api/system.io.directory?view=net-5.0)
- [Como criar e deletar diretórios em C#](https://www.w3schools.com/cs/cs_directories.asp)
- [Tutorial básico de manipulação de diretórios em C#](https://www.devmedia.com.br/trabalhando-com-pastas-e-diretorios-em-csharp/28447)