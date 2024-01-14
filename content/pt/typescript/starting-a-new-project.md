---
title:                "TypeScript: Iniciando um novo projeto"
programming_language: "TypeScript"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Por que começar um novo projeto?

Começar um novo projeto em TypeScript pode ser uma oportunidade emocionante para criar um software moderno e robusto. TypeScript é uma linguagem de programação que combina as características do JavaScript com recursos de tipagem estática, trazendo mais segurança e confiabilidade ao desenvolvimento de software. Ao iniciar um novo projeto em TypeScript, você poderá utilizar todo o potencial dessa linguagem para criar um código mais limpo e escalável.

## Como fazer isso:

Para começar um novo projeto em TypeScript, você precisará instalar o Node.js e o npm (gerenciador de pacotes do Node.js) em seu computador. Em seguida, abra o seu terminal e crie um novo diretório para o seu projeto.

```
mkdir meu-projeto-typescript
cd meu-projeto-typescript
```

Em seguida, inicialize um novo pacote npm em seu diretório com o comando `npm init` e siga as instruções do prompt para criar um arquivo package.json. Isso irá gerenciar as dependências do seu projeto.

Agora, para adicionar o TypeScript ao seu projeto, basta instalar o pacote typescript como uma dependência de desenvolvimento usando o comando:

```
npm install --save-dev typescript
```

Em seguida, crie um arquivo de configuração do TypeScript chamado `tsconfig.json` em seu diretório raiz com o seguinte conteúdo:

```
{
  "compilerOptions": {
    "outDir": "./dist",
    "rootDir": "./src",
    "module": "commonjs",
    "target": "es5",
    "sourceMap": true,
    "esModuleInterop": true,
    "strict": true
  }
}
```

Agora, você pode começar a escrever seu código TypeScript no diretório `src` e o código compilado será gerado no diretório `dist` usando o comando `tsc`.

```
tsc
```

## Profundidade:

A configuração `tsconfig.json` permite que você personalize as opções de compilação do TypeScript para o seu projeto. Existem várias opções disponíveis para você ajustar o comportamento do compilador, como a versão do JavaScript alvo, o modo de módulo, o mapa de origem e a verificação estrita do código. Você pode encontrar mais informações sobre essas opções na [documentação oficial do TypeScript](https://www.typescriptlang.org/docs/handbook/compiler-options.html).

Além disso, ao iniciar um novo projeto em TypeScript, é importante seguir as boas práticas de programação e manter um código limpo e organizado. Isso inclui o uso de interfaces, tipagem de dados adequada e comentários claros para facilitar a leitura e manutenção do código.

## Veja também:

- [Guia oficial do TypeScript](https://www.typescriptlang.org/docs/handbook/intro.html)
- [Documentação do Node.js](https://nodejs.org/en/docs/)
- [Documentação do npm](https://docs.npmjs.com/)