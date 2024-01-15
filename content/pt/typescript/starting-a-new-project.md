---
title:                "Iniciando um novo projeto"
html_title:           "TypeScript: Iniciando um novo projeto"
simple_title:         "Iniciando um novo projeto"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Por que iniciar um novo projeto com TypeScript?

Se você está procurando por uma linguagem de programação versátil e eficiente para iniciar um novo projeto, o TypeScript pode ser a escolha perfeita para você. Além de ser uma linguagem de código aberto, o TypeScript oferece a vantagem de poder ser usado tanto para desenvolvimento frontend como backend, proporcionando uma experiência de desenvolvimento consistente em todo o seu projeto.

## Como começar um projeto com TypeScript
Para começar um novo projeto com TypeScript, siga estes passos simples:

1. Certifique-se de ter o Node.js e o TypeScript instalados em sua máquina.
2. Crie uma pasta para o seu projeto e navegue até ela no terminal.
3. Execute o comando `npm init` para inicializar um novo projeto npm.
4. Instale o TypeScript como dependência de desenvolvimento com o comando `npm install -D typescript`.
5. Crie um arquivo `tsconfig.json` para configurar o seu projeto. Dentro dele, adicione os parâmetros básicos como `target`, `module` e `outDir`.
6. Crie um arquivo `index.ts` e comece a escrever seu código TypeScript.
7. Execute o comando `tsc index.ts` para compilar seu código TypeScript em JavaScript.
8. Agora você pode executar o seu código JavaScript com o comando `node index.js` e ver seus resultados.

Veja um exemplo de código TypeScript abaixo:

```TypeScript
const mensagem: string = "Olá mundo!";
console.log(mensagem);
```

Quando este código for compilado e executado, você verá a seguinte saída:

```
Olá mundo!
```

## Profundidade em iniciar um novo projeto com TypeScript
Ao iniciar um novo projeto com TypeScript, é importante entender alguns conceitos-chave, como o sistema de tipo estático e a compilação de código. O TypeScript é uma linguagem de programação baseada em JavaScript que adiciona recursos de tipagem estática e objetos de classe, proporcionando uma experiência de programação mais robusta e escalável. Além disso, a compilação de código TypeScript para JavaScript garante compatibilidade com navegadores e ambientes de execução.

## Veja também
- [Tutorial do TypeScript](https://www.typescriptlang.org/docs/handbook/typescript-in-5-minutes.html)
- [Documentação oficial do TypeScript](https://www.typescriptlang.org/docs/)
- [Repositório GitHub do TypeScript](https://github.com/Microsoft/TypeScript)