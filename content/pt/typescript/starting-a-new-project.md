---
title:                "TypeScript: Iniciando um novo projeto"
simple_title:         "Iniciando um novo projeto"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Por que iniciar um novo projeto em TypeScript?

Iniciar um novo projeto em TypeScript pode ser benéfico por várias razões, incluindo a capacidade de escrever código mais limpo e organizado, a possibilidade de detectar erros durante a fase de desenvolvimento e a facilidade de integração com outras bibliotecas e frameworks JavaScript.

## Como começar

Para iniciar um novo projeto em TypeScript, siga estes passos simples:

1. Instale o TypeScript em seu computador usando o gerenciador de pacotes de sua escolha (npm, Yarn, etc.).

2. Crie uma pasta para o seu projeto e navegue até ela no seu terminal.

3. Execute o seguinte comando para iniciar um novo projeto em TypeScript:

```TypeScript
    tsc --init
```

Este comando irá criar um arquivo chamado `tsconfig.json` que contém as configurações básicas para o seu projeto em TypeScript.

4. Crie um arquivo `index.ts` dentro da sua pasta do projeto e comece a escrever seu código TypeScript!

## Investigação detalhada

Ao iniciar um novo projeto em TypeScript, é importante entender as configurações no arquivo `tsconfig.json` para personalizar o seu ambiente de desenvolvimento. Aqui estão algumas configurações importantes que você deve conhecer:

- `target`: esta configuração define qual versão do JavaScript será criada pelo compilador do TypeScript. A opção `es5` é recomendada para garantir uma ampla compatibilidade.

- `strict`: ao definir essa configuração como `true`, você habilitará todas as configurações estritas do TypeScript, o que ajudará a evitar erros comuns no código.

- `outDir`: esta configuração define o diretório de saída para os arquivos JavaScript compilados. É recomendado criar uma pasta separada para os arquivos JavaScript compilados a fim de manter seu projeto organizado.

Aprofundar-se mais nessas configurações e em outras pode ajudá-lo a otimizar seu ambiente de desenvolvimento em TypeScript.

## Veja também

- [Documentação do TypeScript](https://www.typescriptlang.org/docs/)
- [Tutorial do TypeScript para iniciantes](https://www.youtube.com/watch?v=BwuLxPH8IDs&ab_channel=CodelyTV)
- [Curso de TypeScript da Udemy](https://www.udemy.com/course/typescript-pt/)