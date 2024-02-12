---
title:                "Usando um depurador"
aliases:
- /pt/google-apps-script/using-a-debugger/
date:                  2024-02-01T22:03:05.981210-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando um depurador"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/google-apps-script/using-a-debugger.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O que & Por quê?

Depurar no Google Apps Script (GAS) envolve o processo de identificar e remover erros de scripts destinados a automatizar os Google Apps ou construir aplicações web. Programadores depuram para garantir que seu código seja executado conforme o esperado, aumentando a confiabilidade e o desempenho nas aplicações.

## Como fazer:

O Google Apps Script oferece um depurador integrado no Editor de Apps Script para ajudar a solucionar problemas nos scripts. Veja como iniciar e usar o depurador:

1. **Abra seu script no Editor de Apps Script.**
2. **Selecione uma função para depurar.** No menu suspenso no topo, selecione a função que deseja depurar.
3. **Defina pontos de interrupção.** Clique na área de margem (a área cinza à esquerda dos números das linhas) onde deseja pausar a execução; um ponto vermelho aparece, indicando um ponto de interrupção.
4. **Inicie a depuração.** Clique no ícone de inseto ou selecione `Depurar` > `Iniciar depuração`. A execução iniciará e pausará no primeiro ponto de interrupção.

Considere este script simples:

```javascript
function calculateSum() {
  var a = 5;
  var b = 10;
  var sum = a + b;
  Logger.log(sum); // Destinado a registrar 15
}
```

Se não tiver certeza do motivo pelo qual `Logger.log(sum)` não está exibindo o resultado esperado, você poderia definir um ponto de interrupção na linha `var sum = a + b;` e avançar pelo script linha por linha para inspecionar os valores das variáveis.

**Saída de exemplo no Logger:**

```plain
15
```

Enquanto depura, o Editor de Apps Script permite que você:

- **Avance pelo código** usando os botões de avançar, entrar e sair.
- **Observe expressões e variáveis** para ver seus valores mudando em tempo real.
- **Inspecione a pilha de chamadas** para rastrear chamadas de função.

## Aprofundando-se

Depurar no Google Apps Script, como em qualquer outro ambiente de programação, é essencial para criar aplicações livres de erros. Introduzido no início do desenvolvimento do GAS, o depurador integrado oferece capacidades fundamentais para inspecionar e corrigir o código incrementalmente. Embora ele forneça recursos básicos de depuração semelhantes aos encontrados em ambientes mais maduros como Visual Studio Code ou IntelliJ, pode ser insuficiente para cenários de depuração complexos. Por exemplo, suas capacidades para inspecionar callbacks assíncronos ou gerenciar execuções de scripts pesados podem ser limitantes.

Para necessidades de depuração complexas, desenvolvedores podem recorrer a métodos alternativos, como registro extensivo (usando `Logger.log()`) ou até mesmo implantando como um aplicativo web para inspecionar comportamento em um cenário do mundo real. No entanto, a simplicidade e integração do depurador do GAS dentro do Editor de Apps Script o tornam um primeiro passo inestimável para solução de problemas e compreensão do comportamento do script. Notavelmente, com as atualizações e melhorias contínuas da Google para o Apps Script, a experiência de depuração está melhorando constantemente, oferecendo ferramentas e opções mais sofisticadas ao longo do tempo. Essa evolução reflete o compromisso da Google em tornar o Apps Script uma plataforma mais poderosa e acessível para desenvolvedores de diversos contextos.
