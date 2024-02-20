---
date: 2024-01-26 04:11:08.800109-07:00
description: "Um depurador \xE9 uma ferramenta que permite examinar e alterar os mecanismos\
  \ internos do seu c\xF3digo enquanto ele \xE9 executado. Os programadores usam para\u2026"
lastmod: 2024-02-19 22:05:05.362170
model: gpt-4-0125-preview
summary: "Um depurador \xE9 uma ferramenta que permite examinar e alterar os mecanismos\
  \ internos do seu c\xF3digo enquanto ele \xE9 executado. Os programadores usam para\u2026"
title: Usando um depurador
---

{{< edit_this_page >}}

## O Que & Porquê?
Um depurador é uma ferramenta que permite examinar e alterar os mecanismos internos do seu código enquanto ele é executado. Os programadores usam para eliminar erros, avançando passo a passo pelo seu código, inspecionando variáveis e compreendendo o fluxo do seu programa.

## Como Fazer:

Para começar a usar um depurador em TypeScript, tudo o que você precisa é de uma IDE suportada (como o Visual Studio Code) e uma configuração `launch.json`. Aqui está um exemplo rápido para uma aplicação Node.js:

```TypeScript
// app.ts
function greet(name: string) {
    console.log(`Olá, ${name}!`);
}

const userName = 'Ada';
greet(userName);
```

Para depurar isso, crie um arquivo `launch.json` sob a pasta `.vscode`:

```JSON
{
    "version": "0.2.0",
    "configuration": [
        {
            "type": "node",
            "request": "launch",
            "name": "Iniciar Programa",
            "skipFiles": ["<node_internals>/**"],
            "program": "${workspaceFolder}/app.ts",
            "preLaunchTask": "tsc: build - tsconfig.json",
            "outFiles": ["${workspaceFolder}/build/**/*.js"]
        }
    ]
}
```

Depois, defina um ponto de interrupção na sua função `greet` clicando do lado esquerdo do número da linha no seu IDE. Pressione F5 para começar a depurar e observe sua aplicação pausar no ponto de interrupção. Agora você pode passar o mouse sobre as variáveis, observar expressões e avançar pelo seu código com facilidade.

## Mergulho Profundo

Antigamente, antes dos ambientes de desenvolvimento integrado (IDEs) se tornarem sofisticados, a depuração era frequentemente realizada com declarações de impressão (a.k.a depuração `console.log`). Funcionava, mais ou menos, mas era como tentar encontrar uma agulha no palheiro vendado.

Os depuradores modernos são como um canivete suíço para a solução de problemas. Com a evolução do TypeScript e do Node.js, existem vários depuradores disponíveis, desde o inspetor embutido do Node.js até as ferramentas de desenvolvedor do navegador para depuração do lado do cliente.

O inspetor do Node.js funciona anexando-se à sua aplicação em execução; ele se comunica através do Protocolo do DevTools do Chrome, transformando seu navegador Chrome em um poderoso console de depuração. Esta integração permite uma sessão de depuração visualmente interativa e detalhada, em comparação com as tradicionais práticas de depuração via linha de comando.

## Veja Também

Para uma leitura extra e algumas dicas profissionais, confira:

- [Depuração de TypeScript no Visual Studio Code](https://code.visualstudio.com/docs/typescript/typescript-debugging)
- [Guia de Depuração do Node.js](https://nodejs.org/en/docs/guides/debugging-getting-started/)
- [Documentação do Chrome DevTools](https://developers.google.com/web/tools/chrome-devtools)
