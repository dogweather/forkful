---
title:                "Removendo aspas de uma string"
aliases:
- /pt/javascript/removing-quotes-from-a-string.md
date:                  2024-01-26T03:40:11.814868-07:00
model:                 gpt-4-0125-preview
simple_title:         "Removendo aspas de uma string"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Remover aspas de uma string significa se livrar dessas incômodas marcas de cotação que podem bagunçar seu código, especialmente quando você está analisando dados ou construindo objetos JSON. Programadores fazem isso para higienizar entradas, evitar erros de sintaxe e fazer com que strings se integrem bem com outras partes do seu código.

## Como Fazer:
Imagine que você tem uma string envolvida em aspas duplas, como `"\"Olá, Mundo!\""` e você quer o texto puro, sem as aspas. Aqui está um rápido trecho de código JavaScript para libertar sua string das algemas das aspas:

```javascript
let quotedString = "\"Olá, Mundo!\"";
let unquotedString = quotedString.replace(/^"|"$/g, '');
console.log(unquotedString); // Saída: Olá, Mundo!
```

E se você estiver lidando com aspas simples? Apenas ajuste um pouco a regex:

```javascript
let singleQuotedString = "'Olá, Mundo!'";
let unquotedString = singleQuotedString.replace(/^'|'$/g, '');
console.log(unquotedString); // Saída: Olá, Mundo!
```

Ou e se sua string for uma mistura de ambos? Sem problemas:

```javascript
let mixedQuotedString = "\"'Olá, Mundo!'\"";
let unquotedString = mixedQuotedString.replace(/^["']|["']$/g, '');
console.log(unquotedString); // Saída: 'Olá, Mundo!'
```

## Aprofundando
Antes de o JSON se popularizar, escapar aspas era um faroeste de barras invertidas e truques. Linguagens de programação antigas nem sempre se davam bem com aspas, o que significava muita manipulação manual de string. Agora, com formatos de dados padronizados, remover aspas muitas vezes é sobre limpar entradas antes de serem processadas como JSON ou armazenar texto sem conflitos de formatação.

Alternativas para `.replace()`? Claro! Você poderia dividir e juntar uma string em aspas, usar slice se você tem certeza das posições das suas aspas, ou até mesmo regex match para extrair o texto necessário. Tudo depende do contexto.

Mas não se esqueça dos casos de borda: aspas dentro de aspas, aspas escapadas e caracteres internacionais. Pense na sua string como um campo minado potencial de exceções, e pise com cuidado. Motores JavaScript modernos são otimizados para lidar eficientemente com operações regex, então geralmente são a primeira opção, mas sempre vale a pena verificar o desempenho para tarefas de processamento de dados pesados.

## Veja Também
Aprofunde-se em manipulação de string e regex:

- Rede de Desenvolvedores Mozilla sobre String.replace(): https://developer.mozilla.org/pt-BR/docs/Web/JavaScript/Reference/Global_Objects/String/replace
- Regex101 para testar seus padrões regex: https://regex101.com/
- JSON.org para entender por que lidamos com tantas aspas no desenvolvimento web moderno: http://json.org/
