---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:09:57.954784-07:00
description: "Arrays associativos, ou dicion\xE1rios em C#, permitem armazenar e gerenciar\
  \ pares de chaves e valores. Eles s\xE3o sua melhor op\xE7\xE3o quando voc\xEA precisa\
  \ buscar\u2026"
lastmod: '2024-03-13T22:44:46.577149-06:00'
model: gpt-4-0125-preview
summary: "Arrays associativos, ou dicion\xE1rios em C#, permitem armazenar e gerenciar\
  \ pares de chaves e valores. Eles s\xE3o sua melhor op\xE7\xE3o quando voc\xEA precisa\
  \ buscar\u2026"
title: Utilizando arrays associativos
---

{{< edit_this_page >}}

## O que & Por quê?

Arrays associativos, ou dicionários em C#, permitem armazenar e gerenciar pares de chaves e valores. Eles são sua melhor opção quando você precisa buscar valores rapidamente com base em um identificador único, tornando a gestão de dados uma tarefa fácil em aplicações complexas.

## Como fazer:

Em C#, você trabalha com arrays associativos usando a classe `Dictionary<TKey, TValue>`. Aqui está um exemplo rápido para começar:

```C#
using System;
using System.Collections.Generic;

class Program
{
    static void Main()
    {
        // Criando um dicionário
        Dictionary<string, int> fruitBasket = new Dictionary<string, int>();

        // Adicionando pares chave-valor
        fruitBasket.Add("Apples", 5);
        fruitBasket.Add("Oranges", 10);

        // Acessando um valor usando sua chave
        Console.WriteLine("Apples: " + fruitBasket["Apples"]);
        
        // Atualizando um valor
        fruitBasket["Apples"] = 7;
        Console.WriteLine("Updated Apples: " + fruitBasket["Apples"]);
        
        // Removendo um par chave-valor
        fruitBasket.Remove("Oranges");

        // Iterando sobre o dicionário
        foreach (var pair in fruitBasket)
        {
            Console.WriteLine(pair.Key + ": " + pair.Value);
        }
    }
}
```
Saída de Exemplo:
```
Apples: 5
Updated Apples: 7
Apples: 7
```

Este exemplo mostra como criar um dicionário, adicionar, acessar, atualizar e remover elementos, e iterar sobre ele.

## Aprofundamento

O conceito de arrays associativos remonta ao seu uso em linguagens de script como Perl e PHP, onde oferecem flexibilidade na gestão de coleções de dados. Em C#, `Dictionary<TKey, TValue>` é a implementação de facto, introduzida no .NET Framework 2.0. Ele armazena dados em uma tabela hash, garantindo buscas, adições e deleções eficientes.

No entanto, vale ressaltar que, embora os dicionários sejam incrivelmente versáteis, eles podem não ser sempre a melhor opção. Para manter coleções ordenadas, você pode considerar `SortedDictionary<TKey, TValue>` ou `SortedList<TKey, TValue>`, que oferecem ordem classificada a custo de operações de inserção e remoção mais lentas. Para cenários que exigem segurança entre threads, `ConcurrentDictionary<TKey, TValue>` adiciona sobrecarga mas garante acesso seguro de múltiplos threads sem bloqueio manual.

Em última análise, a escolha de uma implementação de array associativo em C# depende das suas necessidades específicas quanto à ordem, performance e segurança entre threads.
