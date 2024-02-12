---
title:                "Encontrando o comprimento de uma string"
aliases: - /pt/google-apps-script/finding-the-length-of-a-string.md
date:                  2024-02-01T21:53:58.002080-07:00
model:                 gpt-4-0125-preview
simple_title:         "Encontrando o comprimento de uma string"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/google-apps-script/finding-the-length-of-a-string.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Que?
Encontrar o comprimento de uma string no Google Apps Script, uma linguagem de scripting em nuvem JavaScript que permite automatizar tarefas em produtos do Google, trata-se de determinar o número de caracteres que uma string contém. Programadores frequentemente realizam essa operação para verificar entrada de dados, percorrer caracteres ou manipular strings para várias tarefas de automação dentro dos Apps do Google.

## Como fazer:
No Google Apps Script, você pode encontrar o comprimento de uma string usando a propriedade `.length`, similar ao JavaScript. Esta propriedade retorna o número de caracteres dentro da string, incluindo espaços e caracteres especiais. Aqui estão alguns exemplos:

```javascript
// Definir uma string
var text = "Hello, World!";
// Encontrar o comprimento da string
var length = text.length;
// Registrar o comprimento
Logger.log(length); // Saída: 13
```

Em cenários onde você está trabalhando com entrada de usuário a partir de Formulários ou Planilhas do Google, encontrar o comprimento da string ajuda na validação de dados:

```javascript
// Entrada de string amostra de um usuário no Google Sheets
var userEntry = SpreadsheetApp.getActiveSpreadsheet().getActiveSheet().getRange("A1").getValue();
// Calcular e registrar o comprimento da entrada
Logger.log(userEntry.length); // Saída depende do conteúdo da célula A1
```

Vamos adicionar um exemplo prático que inclui uma condição. Se a entrada exceder um certo comprimento, você talvez queira lançar um erro ou um aviso:

```javascript
var comment = "This is a sample comment that is too long for our database.";
if(comment.length > 50) {
  Logger.log("Erro: Seu comentário não deve exceder 50 caracteres.");
} else {
  Logger.log("Obrigado pela sua submissão.");
}
// Saída: Erro: Seu comentário não deve exceder 50 caracteres.
```

## Aprofundamento
No contexto do Google Apps Script, que é baseado em JavaScript, a propriedade `.length` vem do padrão ECMAScript, que rege as especificações do JavaScript. A propriedade `.length` faz parte do JavaScript desde seus estágios iniciais, fornecendo uma maneira simples de avaliar o tamanho de uma string.

Um detalhe notável é que o Google Apps Script é executado nos servidores do Google, não no navegador. Isso significa que, quando você está lidando com strings e seus comprimentos, especialmente em conjuntos de dados grandes obtidos a partir do Google Sheets ou Docs, o tempo de execução poderia ser afetado devido à latência da rede e às limitações de tempo de execução dos scripts.

Embora `.length` seja um método direto e amplamente utilizado para encontrar o comprimento de uma string, estratégias alternativas podem envolver regex ou iterar através de uma string para contar caracteres, especialmente ao lidar com caracteres multi-byte ou quando você precisa filtrar certos tipos de caracteres. No entanto, para a maioria dos propósitos práticos dentro do Google Apps Script, `.length` oferece uma maneira confiável e eficiente de determinar o comprimento da string.

Sempre lembre, especialmente no Google Apps Script, de considerar o contexto no qual você está executando seu código. Performance e limites de execução podem guiar você a otimizar seus procedimentos de manipulação de strings, incluindo como determinar o comprimento delas.
