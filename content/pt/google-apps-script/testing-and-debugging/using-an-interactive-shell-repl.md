---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:03:43.514195-07:00
description: "Como: O Google Apps Script, uma linguagem de script baseada na nuvem\
  \ para automatizar tarefas em produtos do Google, n\xE3o possui uma ferramenta REPL\u2026"
lastmod: '2024-03-13T22:44:46.109406-06:00'
model: gpt-4-0125-preview
summary: "O Google Apps Script, uma linguagem de script baseada na nuvem para automatizar\
  \ tarefas em produtos do Google, n\xE3o possui uma ferramenta REPL integrada similar\
  \ \xE0s encontradas em linguagens como Python ou o Node.js do JavaScript."
title: Usando uma shell interativa (REPL)
weight: 34
---

## Como:
O Google Apps Script, uma linguagem de script baseada na nuvem para automatizar tarefas em produtos do Google, não possui uma ferramenta REPL integrada similar às encontradas em linguagens como Python ou o Node.js do JavaScript. No entanto, você pode simular uma experiência semelhante usando os recursos de registro e depuração do Editor do Apps Script ou configurando um ambiente externo. Aqui, focamos na criação de um REPL improvisado dentro do editor do Apps Script.

1. **Criando uma função REPL improvisada**:

```javascript
function myREPL() {
  var input = Logger.log('Insira sua expressão: ');
  try {
    var result = eval(input);
    Logger.log('Resultado: ' + result);
  } catch(e) {
    Logger.log('Erro: ' + e.message);
  }
}
```

Como a entrada direta do usuário não é viável da mesma maneira que um REPL tradicional no ambiente do Apps Script, você pode modificar a variável `input` manualmente e executar `myREPL()` para testar expressões.

2. **Execução de Código de Exemplo**:

Suponhamos que você deseje avaliar `2+2`. Você modificaria a função `myREPL` da seguinte forma:

```javascript
function myREPL() {
  var input = '2+2'; // Insira sua expressão manualmente aqui
  // O restante permanece igual...
}
```

Após executar `myREPL()`, verifique os Logs (Ver > Logs) para a saída, que deve ler algo como:

```
[20-xx-xxxx xx:xx:xx:xxx] Insira sua expressão:
[20-xx-xxxx xx:xx:xx:xxx] Resultado: 4
```

3. **Depuração com Logger**:

Para uma depuração mais complexa, intercale `Logger.log(variável);` dentro do seu código para imprimir estados de variáveis, ajudando você a entender o fluxo e estados intermediários dos seus scripts.

## Aprofundamento
O conceito de um REPL é profundamente enraizado na história da computação, originando-se dos sistemas de tempo compartilhado dos anos 1960, que permitiam sessões interativas. Linguagens como Lisp prosperaram nesse ambiente, pois o REPL era crítico para seu processo de desenvolvimento iterativo. Em contraste, o Google Apps Script, surgindo muito mais tarde, é projetado principalmente para a web, focando em automatizar tarefas dentro do conjunto do Google em vez de programação baseada em console iterativo.

O Google Apps Script tradicionalmente não suporta sessões de codificação interativas em tempo real logo de cara devido à sua natureza baseada na nuvem e foco no desenvolvimento de aplicativos web. Seu modelo de execução gira em torno de funções acionadas por eventos da web, gatilhos baseados em tempo ou invocação manual dentro do ambiente, ao invés de loops de feedback instantâneo fornecidos por um REPL.

Enquanto o REPL improvisado e o depurador dentro do Editor do Apps Script oferecem algum nível de interatividade, eles não replicam completamente o feedback imediato e a eficiência dos REPLs tradicionais encontrados em muitas linguagens de programação. Desenvolvedores em busca de uma experiência REPL mais autêntica com tecnologias do Google podem explorar ambientes externos de JavaScript ou Node.js com as APIs do Google. Estes podem fornecer uma sessão de codificação mais responsiva e interativa, embora requeiram mais configuração e, potencialmente, saiam do ambiente direto do Apps Script.
