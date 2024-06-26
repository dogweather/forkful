---
date: 2024-01-26 04:18:36.000737-07:00
description: "Como fazer: TypeScript n\xE3o vem com seu pr\xF3prio REPL. Vamos usar\
  \ o `ts-node`, um ambiente de execu\xE7\xE3o TypeScript para Node.js que inclui\
  \ um REPL. Primeiro,\u2026"
lastmod: '2024-03-13T22:44:46.327712-06:00'
model: gpt-4-0125-preview
summary: "TypeScript n\xE3o vem com seu pr\xF3prio REPL."
title: Usando um shell interativo (REPL)
weight: 34
---

## Como fazer:
TypeScript não vem com seu próprio REPL. Vamos usar o `ts-node`, um ambiente de execução TypeScript para Node.js que inclui um REPL.

Primeiro, instale-o globalmente:
```bash
npm install -g ts-node
```

Inicie o REPL digitando `ts-node` na sua linha de comando:
```bash
ts-node
```

Aqui está um trecho rápido para experimentar:
```TypeScript
> let message: string = 'Olá, REPL!';
> console.log(message);
Olá, REPL!
> 
```
Para encerrar a sessão, pressione `Ctrl+D`.

## Aprofundamento
Historicamente, REPLs eram proeminentes em linguagens como Lisp, permitindo a avaliação dinâmica de código. O conceito desde então se espalhou, tornando-se um elemento básico para codificação interativa em muitas linguagens.

Para TypeScript, `ts-node` não é sua única opção. Alternativas incluem usar o TypeScript Playground em um navegador web ou aproveitar outros REPLs baseados em Node.js que suportam TypeScript com plugins adequados.

Em termos de implementação, o `ts-node` usa a API do compilador TypeScript para transpilar o código on-the-fly antes de ser executado pelo Node.js. Isso oferece feedback imediato e é particularmente útil para experimentar os recursos mais recentes do TypeScript sem as complicações de configuração.

Uma coisa a lembrar – enquanto um REPL é ótimo para testes rápidos, ele não substitui a escrita de código tradicional, testável e mantível. É uma ferramenta para aprendizado e exploração, não um substituto para práticas de desenvolvimento adequadas.

## Veja Também
- [Site Oficial do TypeScript](https://www.typescriptlang.org/)
- [ts-node no GitHub](https://github.com/TypeStrong/ts-node)
- [Documentação do REPL do Node.js](https://nodejs.org/api/repl.html)
- [Playground do TypeScript](https://www.typescriptlang.org/play)
