---
date: 2024-01-20 18:03:11.744288-07:00
description: 'Como Fazer: Para iniciar um novo projeto C#, primeiro garanta de ter
  o .NET SDK instalado. Depois, abra seu terminal favorito.'
lastmod: '2024-03-13T22:44:46.584812-06:00'
model: gpt-4-1106-preview
summary: Para iniciar um novo projeto C#, primeiro garanta de ter o .NET SDK instalado.
title: Iniciando um novo projeto
weight: 1
---

## Como Fazer:
Para iniciar um novo projeto C#, primeiro garanta de ter o .NET SDK instalado. Depois, abra seu terminal favorito.

### Criando um Novo Console App:
```C#
dotnet new console -o MeuApp
cd MeuApp
dotnet run
```
**Saída esperada:**
```
Hello, World!
```

### Iniciando um Projeto Web (ASP.NET Core):
```C#
dotnet new webapp -o MeuWebApp
cd MeuWebApp
dotnet run
```
Abra seu navegador e visite `http://localhost:5000` para ver o resultado.

## Mergulho Profundo
Criar um projeto C# tem sido uma prática desde sua introdução em 2000. Antigamente, se usava o Visual Studio para tal, mas com a chegada do .NET Core e depois o .NET 5 (e posteriores), a linha de comando tornou-se uma opção poderosa e leve.

Há alternativas como usar IDEs (Integrated Development Environment), por exemplo, Visual Studio, Rider, ou VS Code com a extensão C#. Cada um tem seus prós e contras, mas a linha de comando é uma maneira direta e rápida de começar sem dependências extras.

Ao criar seu projeto pela linha de comando, o .NET CLI usa templates que podem ser estendidos ou customizados. Isso facilita a criação de projetos que já vêm com boilerplate code (códigos predefinidos) necessários para iniciar rapidamente.

## Veja Também
- Documentação oficial do .NET CLI: https://docs.microsoft.com/pt-br/dotnet/core/tools/
- Informações sobre templates do .NET: https://docs.microsoft.com/pt-br/dotnet/core/tools/dotnet-new
- Tutorial de ASP.NET Core: https://docs.microsoft.com/pt-br/aspnet/core/getting-started/
- Visual Studio Code: https://code.visualstudio.com/
- JetBrains Rider: https://www.jetbrains.com/rider/
