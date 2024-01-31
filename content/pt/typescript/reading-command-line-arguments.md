---
title:                "Lendo argumentos da linha de comando"
date:                  2024-01-20T17:57:23.736260-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lendo argumentos da linha de comando"

category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## O Que é & Por Que Fazer?
Ler argumentos da linha de comando permite que seu programa TypeScript receba inputs externos ao ser executado. Programadores fazem isso para tornar seus aplicativos mais flexíveis e interativos, permitindo diferentes comportamentos baseados nos dados fornecidos pelos usuários na execução.

## Como Fazer:
```TypeScript
// Instalar a deno se ainda não estiver instalado:
// https://deno.land/#installation

// Salve este código como example.ts
const argumentos = Deno.args; // Deno.args é onde os argumentos da linha de comando são armazenados

console.log("Argumentos Recebidos:");
for (const arg of argumentos) {
  console.log(arg);
}

// Executar o arquivo:
// deno run example.ts arg1 arg2 arg3

// Saída esperada:
// Argumentos Recebidos:
// arg1
// arg2
// arg3
```
## Mergulho Profundo:
A história de ler argumentos da linha de comando remonta aos primórdios da informática, quando os GUIs (Graphical User Interfaces) eram inexistentes e tudo era feito no terminal. No Node.js, teríamos usado `process.argv` para obter argumentos. O Deno, uma alternativa moderna ao Node.js, incorpora muitas de suas funcionalidades mas com um foco maior em segurança e simplicidade, incluindo uma API para argumentos da linha de comando incorporada e pronta para uso, a `Deno.args`.

Quanto à implementação, no Deno, os argumentos da linha de comando não requerem nenhuma permissão especial e podem ser facilmente acessados via `Deno.args`, que retorna um array de strings. Fica a seu critério, enquanto desenvolvedor, validar e analisar esses argumentos para adequá-los às necessidades do seu programa.

Existem bibliotecas de terceiros como `cliffy` e `yargs` que podem fornecer funcionalidades mais avançadas para processar os argumentos da linha de comando, tais como definição de opções, comandos e validação automática.

## Veja Também:
- Documentação oficial do Deno sobre argumentos da linha de comando: https://deno.land/manual/runtime/program_lifecycle#program-arguments
- Guia Node.js sobre argumentos da linha de comando: https://nodejs.dev/learn/nodejs-accept-arguments-from-the-command-line
- Biblioteca `cliffy` para Deno, caso precise de argument parsing mais sofisticado: https://deno.land/x/cliffy
- Biblioteca `yargs` para Node.js, uma solução poderosa para parsing de argumentos: https://www.npmjs.com/package/yargs
