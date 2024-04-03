---
date: 2024-01-26 01:09:23.563546-07:00
description: "Como fazer: Imagine que voc\xEA tem um c\xF3digo que imprime uma sauda\xE7\
  \xE3o v\xE1rias vezes. Sem fun\xE7\xF5es, \xE9 uma bagun\xE7a. Com fun\xE7\xF5es,\
  \ fica arrumado."
lastmod: '2024-03-13T22:44:46.589484-06:00'
model: gpt-4-1106-preview
summary: "Imagine que voc\xEA tem um c\xF3digo que imprime uma sauda\xE7\xE3o v\xE1\
  rias vezes."
title: "Organizando o c\xF3digo em fun\xE7\xF5es"
weight: 18
---

## Como fazer:
Imagine que você tem um código que imprime uma saudação várias vezes. Sem funções, é uma bagunça. Com funções, fica arrumado.

```C#
// Sem funções - repetitivo
Console.WriteLine("Olá, Amy!");
Console.WriteLine("Olá, Bob!");
Console.WriteLine("Olá, Charlie!");

// Com funções - mais limpo
void Saudar(string nome) {
    Console.WriteLine($"Olá, {nome}!");
}

Saudar("Amy");
Saudar("Bob");
Saudar("Charlie");
```

A saída é a mesma, mas a segunda versão é muito mais organizada.

## Mergulho Profundo
Lá atrás, nos dias da linguagem assembly, você pulava para diferentes pontos do código com GOTO—caótico e difícil de acompanhar. Funções são um avanço significativo, como gavetas organizadas em uma caixa de ferramentas. Alternativas? Claro. Temos métodos, que são funções em um contexto de classe. Depois existem as lambdas e funções embutidas para tarefas rápidas e únicas.

Sobre a implementação—funções pequenas e focadas são ouro. São mais fáceis de testar e depurar. Funções grandes com muitas responsabilidades podem se tornar monstruosas, ganhando o título duvidoso de "código espaguete". Mantenha-se à regra de uma tarefa por função; você agradecerá a si mesmo mais tarde.

## Veja Também
Para mais sobre funções e melhores práticas, confira:

- Código Limpo por Robert C. Martin: Princípios para manter suas funções organizadas.
- Refatoração por Martin Fowler: Maneiras de melhorar o código existente.
- Guia de Métodos C# da Microsoft: https://docs.microsoft.com/pt-br/dotnet/csharp/programming-guide/classes-and-structs/methods
