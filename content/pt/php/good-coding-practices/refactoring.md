---
date: 2024-01-26 01:50:43.364274-07:00
description: "Refatora\xE7\xE3o \xE9 o processo de reestruturar c\xF3digo de computador\
  \ existente sem alterar seu comportamento externo. Programadores refatoram para\
  \ melhorar\u2026"
lastmod: '2024-03-13T22:44:46.675697-06:00'
model: gpt-4-0125-preview
summary: "Refatora\xE7\xE3o \xE9 o processo de reestruturar c\xF3digo de computador\
  \ existente sem alterar seu comportamento externo. Programadores refatoram para\
  \ melhorar\u2026"
title: "Refatora\xE7\xE3o"
---

{{< edit_this_page >}}

## O que & Por quê?
Refatoração é o processo de reestruturar código de computador existente sem alterar seu comportamento externo. Programadores refatoram para melhorar atributos não funcionais do software, tornando o código mais limpo, mais eficiente e mais fácil de manter.

## Como fazer:
Vamos pegar um trecho clássico de PHP e aplicar um pouco de mágica de refatoração.

Antes da refatoração, nosso código pode parecer assim:

```php
function printOrderDetails($order) {
    foreach ($order as $item) {
        echo "Item: " . $item['name'];
        echo " - Preço: " . $item['price'];
        echo "<br>";
    }
    
    if (!empty($order)) {
        echo "Total: " . array_sum(array_column($order, 'price'));
    }
}
```

Mas podemos refatorar esse código para melhorar sua clareza e modularidade:

```php
function printItem($item) {
    echo "Item: {$item['name']} - Preço: {$item['price']}<br>";
}

function calculateTotal($order) {
    return array_sum(array_column($order, 'price'));
}

function printOrderDetails(array $order) {
    array_walk($order, 'printItem');

    if (!empty($order)) {
        echo "Total: " . calculateTotal($order);
    }
}
```
Ao dividir a função `printOrderDetails` em funções menores, nosso código se torna mais legível e fácil de depurar.

## Aprofundamento
A refatoração tem suas raízes na comunidade de programação Smalltalk do início dos anos 1990 e foi posteriormente popularizada pelo livro seminal de Martin Fowler "Refatoração: Melhorando o Design do Código Existente" (1999). Embora a refatoração possa ser aplicada a qualquer linguagem de programação, a natureza dinâmica do PHP permite alguns desafios e oportunidades únicos.

Alternativas à refatoração podem incluir reescrever o código do zero, o que geralmente é mais arriscado e consome mais tempo. No ecossistema PHP, ferramentas como PHPStan e Rector podem identificar automaticamente e realizar algumas operações de refatoração, respectivamente. Em termos de implementação, manter as refatorações pequenas e testar extensivamente com testes unitários são práticas chave para garantir uma refatoração bem-sucedida sem introduzir bugs.

## Veja também
- Livro de Refatoração de Martin Fowler: https://martinfowler.com/books/refactoring.html
- PHPStan, uma ferramenta de análise estática PHP: https://phpstan.org/
- Rector, uma ferramenta para refatoração automática de código PHP: https://getrector.org/
- Testes unitários em PHP com PHPUnit: https://phpunit.de/
