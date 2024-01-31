---
title:                "Escrevendo no erro padrão"
date:                  2024-01-19
simple_title:         "Escrevendo no erro padrão"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Escrever no erro padrão (`stderr`) é uma forma de enviar mensagens de erro ou diagnóstico em vez de dados de saída regulares (`stdout`). Programadores fazem isso para separar informações de erro do conteúdo principal, facilitando o debug e a análise de logs.

## Como fazer:

```Javascript
// Escrevendo uma mensagem simples para stderr
console.error('Erro: alguma coisa deu errado!');

// Exemplo com uma função que lança um erro
function falhaAoProcessar() {
    // Código que pode falhar
    if (true) { // Substitua por uma condição real de erro
        console.error('Falha no processamento da função.');
    }
}

falhaAoProcessar(); // Chama a função para exemplificar
```

Saída esperada no terminal:

```
Erro: alguma coisa deu errado!
Falha no processamento da função.
```

## Aprofundando:

Historicamente, `stderr` e `stdout` são conceitos que vêm do Unix, distinguindo duas principais correntes de saída de um programa. Alternativas a `console.error` podem incluir o uso de bibliotecas de log que permitem mais configurações, como níveis de severidade e formatos de saída. No Node.js, `process.stderr.write('mensagem\n')` é uma forma de escrever diretamente para o `stderr`, o que pode ser útil para baixo nível de manipulação de saída. Em navegadores, o `console.error` também pode disparar a ferramenta de desenvolvedor para chamar atenção ao erro ocorrido.

## Veja Também:

- Documentação da MDN sobre Console: https://developer.mozilla.org/pt-BR/docs/Web/API/Console/error
- Node.js `process.stderr`: https://nodejs.org/api/process.html#process_process_stderr
- Artigo sobre stdout e stderr: https://www.jstorimer.com/blogs/workingwithcode/7766119-when-to-use-stderr-instead-of-stdout
