---
aliases:
- /pt/google-apps-script/handling-errors/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:55:12.073586-07:00
description: "O tratamento de erros no Google Apps Script \xE9 sobre prever, capturar\
  \ e responder a exce\xE7\xF5es ou erros que ocorrem durante a execu\xE7\xE3o do\
  \ script. Os\u2026"
lastmod: 2024-02-18 23:08:57.724288
model: gpt-4-0125-preview
summary: "O tratamento de erros no Google Apps Script \xE9 sobre prever, capturar\
  \ e responder a exce\xE7\xF5es ou erros que ocorrem durante a execu\xE7\xE3o do\
  \ script. Os\u2026"
title: Gerenciando erros
---

{{< edit_this_page >}}

## O Quê e Por Quê?

O tratamento de erros no Google Apps Script é sobre prever, capturar e responder a exceções ou erros que ocorrem durante a execução do script. Os programadores implementam isso para proteger os scripts contra falhas inesperadas, garantindo aplicações mais suaves e amigáveis ao usuário que podem gerenciar ou registrar erros graciosamente sem falhas abruptas.

## Como fazer:

O Google Apps Script, sendo baseado em JavaScript, nos permite usar a tradicional declaração `try-catch` para o tratamento de erros, junto com `finally` se a limpeza for necessária independentemente de sucesso ou erro.

```javascript
function myFunction() {
  try {
    // Código que pode gerar um erro
    var sheet = SpreadsheetApp.getActiveSheet();
    var data = sheet.getRange("A1").getValue();
    if (data === "") {
      throw new Error("A célula A1 está vazia.");
    }
    Logger.log(data);
  } catch (e) {
    // Código para tratamento de erro
    Logger.log("Erro: " + e.message);
  } finally {
    // Código de limpeza, executado quer um erro tenha ocorrido ou não
    Logger.log("Função concluída.");
  }
}
```

Saída de amostra sem erro:
```
[Valor da célula]
Função concluída.
```

Saída de amostra com um erro (assumindo que A1 está vazio):
```
Erro: A célula A1 está vazia.
Função concluída.
```

O Google Apps Script também suporta a geração de erros personalizados usando o objeto `Error` e a captura de tipos específicos de erro se necessário. No entanto, a ausência de categorização avançada de erros torna essencial confiar em mensagens de erro para especificidade.

## Mergulho Profundo

Historicamente, o tratamento de erros em linguagens de script como JavaScript (e, por extensão, Google Apps Script) tem sido menos sofisticado do que em algumas linguagens compiladas, que oferecem recursos como hierarquias de exceções detalhadas e ferramentas de depuração abrangentes. O modelo do Google Apps Script é relativamente direto, aproveitando o paradigma `try-catch-finally` do JavaScript. Essa simplicidade alinha-se com o design da linguagem para desenvolver e implantar rapidamente aplicações de pequena a média escala dentro do ecossistema do Google, mas às vezes pode limitar desenvolvedores que lidam com cenários de erro complexos.

Em aplicações mais complexas, programadores frequentemente complementam o tratamento de erros nativo do Google Apps Script com mecanismos personalizados de registro de erros e relatórios de erros. Isso pode incluir escrever erros em uma Planilha Google para auditoria ou usar serviços de registro de terceiros por meio dos Serviços de Fetch de URL do Google Apps Script para enviar detalhes de erros para fora do ambiente de script.

Embora o Google Apps Script possa ficar atrás de linguagens como Java ou C# em termos de complexidade e capacidades de tratamento de erros inerentes, sua integração com os serviços do Google e a simplicidade da abordagem `try-catch-finally` tornam-no uma ferramenta poderosa para desenvolvedores automatizar rapidamente tarefas e criar integrações dentro do ecossistema do Google. Desenvolvedores de outras origens podem encontrar o desafio não em dominar padrões complexos de tratamento de erros, mas em alavancar criativamente o que está disponível para garantir que seus scripts sejam robustos e amigáveis ao usuário.
