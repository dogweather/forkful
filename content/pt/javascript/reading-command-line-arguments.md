---
title:                "Lendo argumentos da linha de comando"
html_title:           "Javascript: Lendo argumentos da linha de comando"
simple_title:         "Lendo argumentos da linha de comando"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## O que e por que?
Ler argumentos da linha de comando é uma forma de obter informações do usuário diretamente na linha de comando, sem a necessidade de um formulário ou interface gráfica. Isso pode ser útil para passar informações importantes ao executar um programa ou script. Programadores usam a leitura de argumentos da linha de comando para tornar seus programas mais flexíveis e fáceis de usar.

## Como fazer:
```Javascript
// Exemplo de um programa que lê argumentos da linha de comando

// Criando uma variável para armazenar os argumentos
let args = process.argv;

// Verificando se foram passados argumentos suficientes
if (args.length < 3) {
  console.log("É necessário passar pelo menos um argumento.");
} else {
  // Iterando sobre os argumentos e imprimindo-os na tela
  for (let i = 2; i < args.length; i++) {
    console.log(`Argumento ${i - 1}: ${args[i]}`);
  }
}

// Exemplo de saída no console
$ node script.js argumento1 argumento2 argumento3
Argumento 1: argumento1
Argumento 2: argumento2
Argumento 3: argumento3
```

## Mergulho profundo:
A leitura de argumentos da linha de comando não é uma prática nova e está presente em diversas linguagens de programação. No entanto, com o avanço das interfaces gráficas, essa habilidade foi perdendo sua relevância. Mesmo assim, é possível encontrar situações em que a leitura de argumentos da linha de comando é necessária, especialmente em scripts e programas que são executados em servidores sem interface gráfica. Alternativas à leitura de argumentos da linha de comando incluem a definição de variáveis de ambiente e a utilização de arquivos de configuração. Na implementação, a leitura de argumentos da linha de comando é realizada através do objeto global do Node.js, ```process```, e o método ```argv``` que retorna um array contendo os argumentos passados.

## Veja também:
- [Documentação oficial do Node.js sobre a leitura de argumentos da linha de comando](https://nodejs.org/docs/latest/api/process.html#process_process_argv)