---
title:                "Organizando o código em funções"
aliases:
- /pt/javascript/organizing-code-into-functions.md
date:                  2024-01-26T01:11:01.780022-07:00
model:                 gpt-4-1106-preview
simple_title:         "Organizando o código em funções"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Organizar o código em funções segmenta tarefas em pedaços reutilizáveis, tornando o código mais limpo e fácil de manter. Fazemos isso para reduzir a redundância, facilitar o teste e melhorar a legibilidade.

## Como fazer:

```javascript
// Define uma função para calcular a área de um retângulo
function calculateArea(largura, altura) {
  return largura * altura;
}

// Chama a função e imprime o resultado
let area = calculateArea(5, 3);
console.log(area); // Saída: 15
```

```javascript
// Agrupa funcionalidades relacionadas usando funções
function cumprimentar(nome) {
  console.log(`Olá, ${nome}!`);
}

function despedir(nome) {
  console.log(`Adeus, ${nome}!`);
}

cumprimentar('Alice'); // Saída: Olá, Alice!
despedir('Bob'); // Saída: Adeus, Bob!
```

## Aprofundando
Historicamente, linguagens de programação imperativas como as primeiras versões de BASIC ou Assembly careciam da abstração que as funções fornecem. Com o passar do tempo, o conceito de código modular em linguagens como C introduziu a ideia de que dividir o código em unidades (funções ou procedimentos) leva a uma melhor organização e lógica mais clara.

Em JavaScript, além das funções comuns, temos funções arrow desde o ES6 (2015), que fornecem uma sintaxe mais concisa e são adequadas para funções que não são métodos.

Alternativas e aprimoramentos na organização do código em JavaScript incluem abordagens orientadas a objetos usando classes, ou paradigmas de programação funcional que tratam as funções como cidadãos de primeira classe.

Em termos de implementação, as funções em JavaScript suportam closures, fornecendo uma maneira de reter acesso ao escopo de uma função após a execução, o que é poderoso para encapsulamento e criação de funções de fábrica, entre outros padrões.

## Veja Também
- MDN Web Docs sobre Funções: https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Guide/Functions
- Padrões de Design em JavaScript: https://addyosmani.com/resources/essentialjsdesignpatterns/book/
- Código Limpo JavaScript: https://github.com/ryanmcdermott/clean-code-javascript
