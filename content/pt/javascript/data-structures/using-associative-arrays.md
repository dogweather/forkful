---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:11:50.092811-07:00
description: "Arrays associativos, ou como s\xE3o mais precisamente conhecidos em\
  \ JavaScript, objetos, permitem mapear chaves a valores. Isso \xE9 extremamente\
  \ \xFAtil quando\u2026"
lastmod: '2024-02-25T18:49:44.572475-07:00'
model: gpt-4-0125-preview
summary: "Arrays associativos, ou como s\xE3o mais precisamente conhecidos em JavaScript,\
  \ objetos, permitem mapear chaves a valores. Isso \xE9 extremamente \xFAtil quando\u2026"
title: Usando arrays associativos
---

{{< edit_this_page >}}

## O Que & Por Quê?

Arrays associativos, ou como são mais precisamente conhecidos em JavaScript, objetos, permitem mapear chaves a valores. Isso é extremamente útil quando você precisa de uma coleção de elementos que deseja acessar através de nomes específicos (chaves) em vez de índices numéricos, tornando seu código mais legível e flexível.

## Como fazer:

Criar e usar arrays associativos (objetos) em JavaScript é direto. Você define um objeto com chaves `{}`, e dentro delas, você pode definir um conjunto de pares chave-valor. As chaves são sempre strings, e os valores podem ser qualquer coisa: strings, números, arrays, até outros objetos.

```javascript
// Criando um array associativo
let userInfo = {
  name: "Alex",
  age: 30,
  email: "alex@example.com"
};

// Acessando elementos
console.log(userInfo.name); // Saída: Alex
console.log(userInfo["email"]); // Saída: alex@example.com

// Adicionando novos elementos
userInfo.job = "Desenvolvedor";
userInfo["country"] = "Canadá";

console.log(userInfo);
/* Saída:
{
  name: "Alex",
  age: 30,
  email: "alex@example.com",
  job: "Desenvolvedor",
  country: "Canadá"
}
*/

// Deletando um elemento
delete userInfo.age;
console.log(userInfo);
/* Saída:
{
  name: "Alex",
  email: "alex@example.com",
  job: "Desenvolvedor",
  country: "Canadá"
}
*/
```

Como você pode ver, acessar, adicionar ou deletar elementos em um array associativo é bastante direto e intuitivo.

## Aprofundando

No mundo JavaScript, embora muitas vezes ouçamos o termo "array associativo", tecnicamente é um equívoco porque JavaScript não possui verdadeiros arrays associativos como outras linguagens (por exemplo, PHP). O que JavaScript tem são objetos que servem a um propósito similar, mas são uma construção mais poderosa e flexível.

Historicamente, arrays em linguagens de programação foram projetados para conter uma coleção de itens, acessados por seu índice numérico. No entanto, à medida que o desenvolvimento de software evoluiu, surgiu a necessidade de estruturas de dados mais flexíveis. Arrays associativos, ou dicionários em outras línguas, foram uma resposta, permitindo acesso a elementos através de chaves arbitrárias.

A abordagem do JavaScript com objetos como armazenamentos de chave-valor oferece uma mistura de funcionalidades. Permite que propriedades (chaves) sejam adicionadas, removidas e consultadas pelo nome. JSON (JavaScript Object Notation) é um testemunho da utilidade dessa estrutura, tornando-se o padrão de fato para troca de dados na web.

Embora objetos cubram a maioria das necessidades para arrays associativos, em casos onde a ordem das chaves ou iteração é importante, o objeto `Map` introduzido no ES6 proporciona uma alternativa melhor. Um `Map` mantém a ordem das chaves, aceita uma gama mais ampla de tipos de dados como chaves e inclui métodos úteis para iteração e recuperação de tamanho. Apesar dessas vantagens, a sintaxe de objeto tradicional permanece popular por sua simplicidade e facilidade de uso em muitos cenários comuns.
