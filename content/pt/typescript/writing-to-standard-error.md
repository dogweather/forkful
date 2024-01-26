---
title:                "Escrevendo no erro padrão"
html_title:           "Arduino: Escrevendo no erro padrão"
simple_title:         "Escrevendo no erro padrão"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## O Que é & Porquê?
Escrever para o erro padrão (stderr) é o processo de envio de mensagens de erro e diagnóstico para um canal de saída especial, separado da saída normal (stdout). Programadores usam isso para reportar erros e para debugar, deixando a saída do programa limpa para resultados válidos.

## Como fazer:

```typescript
// Simple message to stderr
console.error("Oops! Algo deu errado.");

// Formatted error message
const code = 404;
console.error(`Erro: ${code} - Página não encontrada.`);

// Error object
const errorObj = new Error("Erro de Sistema");
console.error(errorObj);
```

Saída de exemplo:
```
Oops! Algo deu errado.
Erro: 404 - Página não encontrada.
Error: Erro de Sistema
```

## Mergulho Profundo
Historicamente, o conceito de separar stderr de stdout vem do Unix, permitindo aos utilizadores direcionar estas saídas de forma independente. Alternativas ao `console.error` em TypeScript incluem escrever diretamente para `process.stderr.write`, o que pode ser mais adequado para customização baixo-nível. A implementação real de `console.error` apenas envia a mensagem de erro para `process.stderr`.

```typescript
// Escrevendo diretamente no stderr
process.stderr.write("Erro crítico!\n");
```

## Veja Também
- [Node.js documentation for console.error](https://nodejs.org/api/console.html#consoleerrordata-args)
- [Node.js process.stderr documentation](https://nodejs.org/api/process.html#processstderr)
- [Understanding Standard Streams](https://en.wikipedia.org/wiki/Standard_streams)
