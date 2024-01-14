---
title:                "C#: Verificando se um diretório existe."
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que verificar se um diretório existe?

Ao escrever um programa em C#, é importante garantir que as operações que o programa realiza não sejam interrompidas por erros causados pela falta de um diretório específico. Verificar se um diretório existe antes de tentar acessá-lo é uma forma simples de evitar erros e garantir que o programa funcione corretamente.

## Como fazer?

Para verificar se um diretório existe em C#, podemos utilizar o método "Directory.Exists" da classe "System.IO". Veja um exemplo de código abaixo:

```C#
string caminhoDiretorio = @"C:\Pasta\Exemplo"; //caminho do diretório que iremos verificar

if(Directory.Exists(caminhoDiretorio))
{
    Console.WriteLine("O diretório existe.");
}
else
{
    Console.WriteLine("O diretório não existe.");
}
```

Caso o diretório exista, o programa irá imprimir "O diretório existe.". Mas, se o diretório não existir, o programa irá imprimir "O diretório não existe.". Assim, podemos tomar medidas adequadas para lidar com a situação.

## Mergulho Profundo

Uma coisa importante a se observar é que o método "Directory.Exists" apenas verifica se o diretório existe, mas não especifica se esse diretório é acessível ou se temos permissões de leitura e/ou escrita nele. Portanto, é importante também lidar com esses casos e implementar tratamentos de erro adequados.

Além disso, é possível também utilizar o método "Directory.GetAccessControl" para obter informações sobre as permissões de acesso ao diretório. Isso pode ser útil para garantir que o programa tenha as permissões necessárias para ler ou escrever no diretório em questão.

## Veja também

- [Documentação oficial do método Directory.Exists do C#](https://docs.microsoft.com/pt-br/dotnet/api/system.io.directory.exists?view=net-5.0)
- [Tutorial completo sobre manipulação de diretórios em C#](https://www.educba.com/c-sharp-directory/)