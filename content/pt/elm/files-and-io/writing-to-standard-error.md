---
title:                "Escrevendo para o erro padrão"
aliases:
- /pt/elm/writing-to-standard-error.md
date:                  2024-02-03T19:33:11.899252-07:00
model:                 gpt-4-0125-preview
simple_title:         "Escrevendo para o erro padrão"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Quê?

Escrever no erro padrão (stderr) é sobre redirecionar mensagens de erro e diagnósticos separadamente da saída principal do programa, que vai para a saída padrão (stdout). Programadores fazem isso para tornar o tratamento de erro e o registro mais gerenciáveis, especialmente em ambientes onde a distinção de saída é crucial para depuração e monitoramento.

## Como fazer:

O Elm é voltado principalmente para o desenvolvimento web, onde o conceito de escrever diretamente para o stderr não se aplica da mesma forma que em ambientes de linha de comando tradicionais. No entanto, para programas Elm rodando no Node.js ou ambientes similares, a interoperação com JavaScript usando portas é a abordagem chave para alcançar uma funcionalidade similar. Aqui está como você pode configurar isso:

Código Elm (`Main.elm`):
```elm
port module Main exposing (main)

import Browser

port errorOut : String -> Cmd msg

-- Exemplo de função fictícia que envia uma mensagem de erro para o JS
generateError : String -> Cmd msg
generateError message =
    errorOut message

main =
    generateError "Esta é uma mensagem de erro para stderr"
```

Interoperação JavaScript (`index.js`):
```javascript
const { Elm } = require('./Main.elm');

var app = Elm.Main.init();

app.ports.errorOut.subscribe((message) => {
  console.error(message);
});
```

Este código Elm define uma porta `errorOut` que permite enviar mensagens do Elm para o JavaScript. Então, no código JavaScript, nós ouvimos por mensagens enviadas através desta porta e as redirecionamos para stderr usando `console.error()`. Desta forma, você pode efetivamente escrever para stderr em um ambiente que o suporte, aproveitando os recursos de interoperação do Elm com o JavaScript.

Saída de exemplo no terminal do Node.js (quando o `index.js` é executado):
```
Esta é uma mensagem de erro para stderr
```
