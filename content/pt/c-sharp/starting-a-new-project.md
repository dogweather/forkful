---
title:                "Iniciando um novo projeto"
html_title:           "Javascript: Iniciando um novo projeto"
simple_title:         "Iniciando um novo projeto"
programming_language: "C#"
category:             "C#"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/starting-a-new-project.md"
---

{{< edit_this_page >}}

# C# Novo Projeto: Entendendo, Criando e Evoluindo Códigos
## O Quê e Por Quê?
Iniciar um novo projeto em C# significa estabelecer as fundações do nosso código. Fazemos isso para criar um ambiente de desenvolvimento específico para nossas soluções, permitindo implementações úteis, eficientes e escaláveis.

## Como fazer:
Aqui estão alguns passos simples para começar um novo projeto C#:

```C#
// Para uma aplicação console em .NET Core
dotnet new console -n MeuAppConsole

// Para uma aplicação web em .NET Core
dotnet new webapp -n MinhaWebApp

// Para uma biblioteca de classes em .NET Core
dotnet new classlib -n MinhaBibliotecaClasses
```
O comando "-n" é seguido pelo nome que você quer dar ao seu projeto. As linhas acima são exemplos e você pode criar o tipo de projeto que melhor atenda às suas necessidades.

## Mergulho profundo:
Historicamente, projetos C# eram geralmente criados usando a IDE do Visual Studio. No entanto, com a chegada do .NET Core, podemos agora criar projetos em qualquer plataforma - Windows, Linux, ou MacOS.

Existem várias alternativas para iniciar um novo projeto, incluindo o uso de IDEs como VS Code ou Jetbrains Rider. Entretanto, o uso do comando `dotnet new` é uma escolha mais rápida e comum.

Os detalhes de implementação irão depender do tipo de aplicação que você está criando. Por exemplo, uma aplicação console terá um método main dentro de uma classe Program, enquanto uma aplicação web pode ter mais complexidade, envolvendo controllers, views e modelos.

## Ver também:
- [Criando um aplicativo .NET Core com linha de comando](https://docs.microsoft.com/pt-br/dotnet/core/tools/dotnet-new)
- [Criando um aplicativo .NET Core com Visual Studio](https://docs.microsoft.com/pt-br/visualstudio/containers/container-tools?view=vs-2019)

Lembre-se, não importa quantos projetos você inicie, o que importa é como eles evoluem e se aperfeiçoam ao longo do tempo. Pratique, experimente e aprenda sempre!