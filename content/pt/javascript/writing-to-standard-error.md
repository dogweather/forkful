---
title:                "Escrevendo para erro padrão"
html_title:           "Javascript: Escrevendo para erro padrão"
simple_title:         "Escrevendo para erro padrão"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

O que & Porquê?
Escrever para o erro padrão (standard error) é um conceito importante na programação. É uma forma de direcionar informações de erro e depuração para um canal diferente do que é usado para a saída usual do programa. Os programadores utilizam essa técnica para facilitar a identificação e correção de erros em seus códigos.

Como Fazer:
Existem duas maneiras principais de escrever para o erro padrão em Javascript: usando o método console.error() e utilizando o objeto process.stderr. Ambos retornam uma mensagem de erro no formato de texto e podem ser usados em conjunto com outros métodos de depuração para melhorar o processo de identificação de erros.

```Javascript
//exemplo usando console.error()
console.error("Houve um erro!");
//saída: Houve um erro!

//exemplo usando process.stderr
process.stderr.write("Houve um erro!");
//saída: Houve um erro!
```

Mergulho Profundo:
A prática de escrever para o erro padrão tem sido usada por programadores há décadas. Antigamente, o procedimento era conhecido como "despejo de núcleo" e envolvia o salvamento de informações do programa em um arquivo para ser analisado posteriormente. Hoje em dia, com o avanço da tecnologia, existem outras alternativas para depurar erros, como a utilização de ferramentas de depuração integradas nos editores de código.

Veja Também:
- Documentação oficial da função console.error: https://developer.mozilla.org/en-US/docs/Web/API/Console/error
- Tutorial sobre a utilização do objeto process.stderr: https://nodejs.org/api/process.html#process_process_stderr
- Artigo sobre a evolução do processo de depuração na programação: https://queue.acm.org/detail.cfm?id=1864069