---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:12:12.132316-07:00
description: "Como fazer: No PHP, criar e usar arrays associativos \xE9 direto. Aqui\
  \ est\xE1 um r\xE1pido resumo."
lastmod: '2024-03-13T22:44:46.660365-06:00'
model: gpt-4-0125-preview
summary: "No PHP, criar e usar arrays associativos \xE9 direto."
title: Usando arrays associativos
weight: 15
---

## Como fazer:
No PHP, criar e usar arrays associativos é direto. Aqui está um rápido resumo:

```PHP
<?php
// Criando um array associativo
$person = array(
    "name" => "John Doe",
    "age" => 30,
    "email" => "john@example.com"
);

// Alternativamente, a sintaxe de array curto
$person = [
    "name" => "John Doe",
    "age" => 30,
    "email" => "john@example.com"
];

// Acessando valores usando chaves
echo "Nome: " . $person["name"] . "\n";
echo "Idade: " . $person["age"] . "\n";
echo "Email: " . $person["email"] . "\n";

// Modificando um valor
$person["age"] = 31;

// Adicionando um novo par chave-valor
$person["country"] = "USA";

// Iterando sobre um array associativo
foreach ($person as $key => $value) {
    echo $key . ": " . $value . "\n";
}

// Saída
// Nome: John Doe
// Idade: 31
// Email: john@example.com
// country: USA
?>
```

Note como as chaves podem ser qualquer string, permitindo que você acesse elementos usando essas chaves em vez de índices numéricos, que podem ser menos significativos e mais difíceis de lembrar.

## Aprofundando
Arrays associativos no PHP são implementados internamente usando tabelas de hash, que fornecem um acesso muito rápido aos elementos por chave, tornando-os altamente eficientes para muitas tarefas. Essa eficiência, combinada com a facilidade de uso, faz dos arrays associativos uma pedra angular da programação PHP.

Historicamente, os arrays do PHP (tanto indexados quanto associativos) têm sido incrivelmente flexíveis, permitindo que eles sirvam como listas, pilhas, filas e mais. No entanto, essa flexibilidade pode às vezes levar a um código menos eficiente se não utilizado com cuidado.

Recentemente, com melhorias na programação orientada a objetos no PHP, alguns desenvolvedores preferem usar objetos para dados estruturados, particularmente para conjuntos de dados complexos ou inter-relacionados. Usar classes pode oferecer melhor encapsulamento e abstração, tornar o código mais fácil de testar e esclarecer intenções. No entanto, para cenários simples de armazenamento de chave-valor e manipulação de dados direta, os arrays associativos continuam sendo uma excelente escolha devido à sua simplicidade e à sintaxe intuitiva.
