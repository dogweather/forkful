---
title:                "Escrevendo no erro padrão"
aliases:
- /pt/google-apps-script/writing-to-standard-error.md
date:                  2024-02-01T22:09:03.486770-07:00
model:                 gpt-4-0125-preview
simple_title:         "Escrevendo no erro padrão"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/google-apps-script/writing-to-standard-error.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Que?

Escrever para o erro padrão (stderr) em linguagens de programação trata-se de direcionar mensagens de erro e diagnósticos para um fluxo separado, distinto da saída padrão (stdout). Programadores fazem isso para dissociar a saída normal do programa das mensagens de erro, tornando a depuração e análise de logs mais direta.

## Como:

Script do Google Apps, sendo uma linguagem de script para desenvolvimento de aplicações leves na plataforma do Google Apps, não oferece uma função embutida direta como `console.error()` para escrever para stderr, como você poderia encontrar em Node.js ou Python. Contudo, você pode simular esse comportamento usando os serviços de registro de logs do Script do Google Apps ou um tratamento de erro personalizado para gerenciar e segregare as saídas de erro.

### Exemplo: Usando `Logger` para Mensagens de Erro

```javascript
function logError() {
  try {
    // Simular um erro
    const result = 1 / 0;
    if(!isFinite(result)) throw new Error("Tentativa de divisão por zero");
  } catch (e) {
    // Escrever a mensagem de erro nos Logs
    Logger.log('Erro: ' + e.message);
  }
}
```

Quando você executar `logError()`, isso escreverá a mensagem de erro no log do Script do Google Apps, que você pode visualizar por `Ver > Logs`. Isso não é exatamente stderr, mas serve a um propósito semelhante de separar logs de erro das saídas padrões.

### Registro Avançado de Diagnósticos

Para depuração e registro de erros mais avançados, você pode usar o Stackdriver Logging, agora conhecido como Google Cloud's Operations Suite.

```javascript
function advancedErrorLogging() {
  try {
    // Causar um erro deliberadamente
    const obj = null;
    const result = obj.someProperty;
  } catch (e) {
    console.error('Erro encontrado: ', e.toString());
  }
}
```

Isso direcionará a mensagem de erro para o Stackdriver Logging, onde é gerenciado como um log de nível de erro. Note que a integração Stackdriver/Google Cloud’s Operations Suite oferece uma solução de registro mais granular e pesquisável em comparação com `Logger`.

## Mergulho Profundo

A falta de um fluxo de `stderr` dedicado no Script do Google Apps reflete sua natureza e origens como uma linguagem de script baseada na nuvem, onde saídas tradicionais de console ou terminal (como stdout e stderr) são menos relevantes. Historicamente, o Script do Google Apps foi projetado para melhorar a funcionalidade do Google Apps com scripts simples, focando na facilidade de uso em detrimento de recursos abrangentes disponíveis em ambientes de programação mais complexos.

Dito isso, a evolução do Script do Google Apps em direção ao desenvolvimento de aplicações mais sofisticadas levou os desenvolvedores a adotar abordagens criativas para tratamento de erros e registro de logs, utilizando os serviços disponíveis como Logger e integrando-se com o Google Cloud’s Operations Suite. Esses métodos, embora não sejam implementações diretas de stderr, oferecem alternativas robustas para gerenciamento de erros e registro de diagnósticos em um ambiente centrado na nuvem.

Crucialmente, enquanto esses métodos servem ao propósito dentro do ecossistema do Script do Google Apps, eles sublinham as limitações da plataforma em comparação a ambientes de programação tradicionais. Para desenvolvedores que requerem estratégias de tratamento de erros detalhadas e hierárquicas, a integração com serviços externos de registro ou a adoção de Google Cloud Functions, que oferecem um tratamento mais convencional de stderr e stdout, pode ser preferível.
