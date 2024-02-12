---
title:                "Lendo argumentos da linha de comando"
aliases: - /pt/google-apps-script/reading-command-line-arguments.md
date:                  2024-02-01T21:58:58.134559-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lendo argumentos da linha de comando"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/google-apps-script/reading-command-line-arguments.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O que & Por quê?

Ler argumentos da linha de comando no Google Apps Script é um pouco inadequado porque, ao contrário de interfaces de linha de comando tradicionais em linguagens de programação como Python ou Node.js, o Google Apps Script não suporta inerentemente a execução de linha de comando ou análise de argumentos. Em vez disso, os programadores muitas vezes simulam esse processo por meio de funções personalizadas e parâmetros de URL ao executar aplicativos web ou tarefas automatizadas, possibilitando interação dinâmica com as funcionalidades do script baseadas em entradas do usuário ou parâmetros predefinidos.

## Como:

Para imitar o processo de leitura dos argumentos da linha de comando no Google Apps Script, especialmente para aplicativos web, você pode utilizar parâmetros de string de consulta. Quando um usuário acessa o URL do aplicativo web, você pode anexar argumentos como `?name=John&age=30` e analisar esses dentro do seu código Apps Script. Veja como você pode configurar isso:

```javascript
function doGet(e) {
  var params = e.parameter; // Recupera os parâmetros da string de consulta
  var name = params['name']; // Obtém o parâmetro 'name'
  var age = params['age']; // Obtém o parâmetro 'age'

  // Saída de exemplo:
  var output = "Nome: " + name + ", Idade: " + age;
  return HtmlService.createHtmlOutput(output);
}

// URL de exemplo: https://script.google.com/macros/s/your_script_id/exec?name=John&age=30
```

Quando você acessa o URL com os parâmetros especificados, o script gera algo como:

```
Nome: John, Idade: 30
```

Essa abordagem é fundamental para criar interações personalizadas em aplicativos web ou controlar programaticamente as execuções de scripts.

## Aprofundamento

Argumentos de linha de comando, como entendidos no contexto das linguagens de programação tradicionais, trazem capacidades para scripts e aplicativos processarem parâmetros em tempo de execução, possibilitando execuções de código flexíveis e dinâmicas baseadas em entrada do usuário ou processos automatizados. O Google Apps Script, sendo uma linguagem de script baseada na nuvem para desenvolvimento de aplicativos leves no ecossistema do Google Workspace, não opera nativamente através de uma interface de linha de comando. Em vez disso, sua execução é em grande parte orientada por eventos ou acionada manualmente através da UI do Apps Script e do Google Workspace, ou via aplicativos web que podem analisar parâmetros de URL como pseudo argumentos de linha de comando.

Dada essa diferença arquitetônica, programadores vindos de um contexto de linguagens com forte uso de CLI podem precisar ajustar sua abordagem ao automatizar tarefas ou desenvolver aplicativos no Google Apps Script. Em vez da análise tradicional de argumentos de linha de comando, a exploração da funcionalidade de aplicativo web do Google Apps Script ou até mesmo funções customizadas do Google Sheets para processamento interativo de dados podem atingir fins semelhantes. Embora isso possa parecer uma limitação a princípio, incentiva o desenvolvimento de interfaces mais amigáveis ​​e aplicativos web acessíveis, alinhando-se ao foco do Google Apps Script na integração e extensão contínua dos aplicativos do Google Workspace.

Para cenários em que a emulação mais próxima do comportamento CLI é primordial (por exemplo, automatizar tarefas com parâmetros dinâmicos), os desenvolvedores poderiam explorar o uso de plataformas externas que chamam aplicativos web do Google Apps Script, passando parâmetros por URLs como um método "linha de comando" improvisado. No entanto, para projetos nativos do Google Apps Script, abraçar o modelo orientado a eventos e centrado na UI do plataforma muitas vezes leva a soluções mais diretas e sustentáveis.
