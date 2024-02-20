---
date: 2024-01-26 03:36:46.490053-07:00
description: "Refatora\xE7\xE3o \xE9 o processo de reestruturar o c\xF3digo de computador\
  \ existente sem alterar seu comportamento externo. Os programadores fazem isso para\
  \ tornar o\u2026"
lastmod: 2024-02-19 22:05:05.366093
model: gpt-4-0125-preview
summary: "Refatora\xE7\xE3o \xE9 o processo de reestruturar o c\xF3digo de computador\
  \ existente sem alterar seu comportamento externo. Os programadores fazem isso para\
  \ tornar o\u2026"
title: "Refatora\xE7\xE3o"
---

{{< edit_this_page >}}

## O Que e Por Que?
Refatoração é o processo de reestruturar o código de computador existente sem alterar seu comportamento externo. Os programadores fazem isso para tornar o código mais limpo, mais fácil de manter e reduzir a complexidade, o que facilita a compreensão para alguém que mergulha de fresco.

## Como fazer:
Considere uma função TypeScript que já viu dias melhores - está um pouco bagunçada, e poderia usar um pouco de carinho e atenção:

```typescript
function userInfo(data: any): string {
    return "User Info: " + data.name + ", " + data.age + ", " + data.email + ";" ;
}
```
Refatorada, isso poderia parecer assim:

```typescript
interface User {
    name: string;
    age: number;
    email: string;
}

function formatUserInfo(user: User): string {
    return `User Info: ${user.name}, ${user.age}, ${user.email};`;
}
```

O segundo exemplo é mais robusto, aproveitando o sistema de tipos do TypeScript com uma `interface` para evitar potenciais erros de tempo de execução e melhorar a legibilidade.

## Mergulho Profundo
Refatoração não é um conceito moderno; evoluiu com a programação, tornando-se mais formalizado com o lançamento do livro "Refactoring: Improving the Design of Existing Code" de Martin Fowler em 1999. É crucial em um ambiente de desenvolvimento Ágil, facilitando mudanças adaptativas no código. Algumas alternativas para a refatoração manual incluem ferramentas automatizadas como TSLint ou o próprio servidor de linguagem do TypeScript que podem sugerir ou até mesmo realizar certas tarefas de refatoração para você. Os detalhes de implementação geralmente envolvem reconhecer "cheiros de código", como código duplicado, métodos longos ou classes grandes, e aplicar padrões para remediar—como extrair métodos, mover para classes mais adequadas ou usar construções mais simples. Esses padrões são chave para entender o como e o porquê da refatoração.

## Veja Também
- [O livro "Refactoring: Improving the Design of Existing Code" por Martin Fowler](https://martinfowler.com/books/refactoring.html)
- [TSLint para análise estática de código](https://palantir.github.io/tslint/)
- [Entendendo Cheiros de Código](https://refactoring.guru/refactoring/smells)
