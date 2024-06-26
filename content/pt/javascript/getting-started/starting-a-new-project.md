---
date: 2024-01-20 18:04:02.907062-07:00
description: 'Como Fazer: Vamos criar um novo projeto simples. Primeiro, inicie um
  projeto Node.js com `npm init`.'
lastmod: '2024-03-13T22:44:46.963249-06:00'
model: gpt-4-1106-preview
summary: Vamos criar um novo projeto simples.
title: Iniciando um novo projeto
weight: 1
---

## Como Fazer:
Vamos criar um novo projeto simples. Primeiro, inicie um projeto Node.js com `npm init`.

```Javascript
// No terminal, digite:
npm init -y
```

Isso vai gerar um arquivo `package.json` padrão. Agora, crie um arquivo `index.js`.

```Javascript
// Abra seu editor de texto e escreva:

console.log('Olá, mundo novo de projetos!');

// Salve como index.js
```

Teste o seu projeto.

```Javascript
// No terminal, execute:

node index.js

// Saída esperada:
Olá, mundo novo de projetos!
```

## Aprofundamento
Quando programadores falam em começar um novo projeto, isso costumava significar a configuração manual de toda a estrutura necessária antes mesmo de escrever o código relacionado ao que o projeto realmente iria fazer. Hoje, com ferramentas como o `npm` ou `yarn` para o ecossistema JavaScript e frameworks com seus próprios CLIs (Interfaces de Linha de Comando), criar um novo projeto é geralmente rápido e padronizado.

Alternativas no ecossistema JavaScript incluem o uso de ferramentas como o `create-react-app` para projetos React ou o `vue-cli` para Vue.js, que configuram muitos aspectos para você. Especificamente para Node.js, a inicialização com `npm init` é simples, mas é apenas o começo—muitos projetos vão requerer dependências adicionais e scripts de automação.

Na implementação, diferentes projetos têm necessidades diferentes; um projeto web pode requerer um servidor de desenvolvimento e ferramentas de compilação como webpack ou Rollup, enquanto um script de automação pode apenas precisar do Node.js instalado e algumas bibliotecas úteis.

## Veja Também:
- Documentação do npm: https://docs.npmjs.com/
- create-react-app: https://reactjs.org/docs/create-a-new-react-app.html
- vue-cli: https://cli.vuejs.org/
- Sobre Node.js: https://nodejs.org/en/about/

Com esses recursos, você tem um ponto de partida para apequenar a curva de aprendizado e potencializar a eficiência do desenvolvimento de seus futuros projetos.
