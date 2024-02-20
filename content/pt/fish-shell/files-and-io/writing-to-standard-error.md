---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:17.123817-07:00
description: "Escrever para o erro padr\xE3o (stderr) no Fish Shell \xE9 sobre direcionar\
  \ mensagens de erro ou diagn\xF3sticos separadamente da sa\xEDda padr\xE3o (stdout).\
  \ Os\u2026"
lastmod: 2024-02-19 22:05:06.090575
model: gpt-4-0125-preview
summary: "Escrever para o erro padr\xE3o (stderr) no Fish Shell \xE9 sobre direcionar\
  \ mensagens de erro ou diagn\xF3sticos separadamente da sa\xEDda padr\xE3o (stdout).\
  \ Os\u2026"
title: "Escrevendo para o erro padr\xE3o"
---

{{< edit_this_page >}}

## O Que & Porquê?

Escrever para o erro padrão (stderr) no Fish Shell é sobre direcionar mensagens de erro ou diagnósticos separadamente da saída padrão (stdout). Os programadores fazem isso para garantir que as informações de erro possam ser facilmente identificadas, gerenciadas ou redirecionadas, facilitando processos mais suaves de depuração e registro.

## Como Fazer:

No Fish Shell, você pode escrever para stderr redirecionando sua saída usando `>&2`. Aqui está um exemplo básico:

```fish
echo "Esta é uma mensagem de erro" >&2
```

Este comando simplesmente ecoa uma mensagem para stderr em vez de stdout. Se você fosse escrever um script que gera tanto mensagens regulares quanto de erro, você poderia fazer algo assim:

```fish
echo "Iniciando o processo"
echo "Ocorreu um erro" >&2
echo "Processo concluído"
```

Saída de exemplo se você executar o script e redirecionar stderr para um arquivo:

```
Iniciando o processo
Processo concluído
```

A mensagem de erro não apareceria na saída padrão, mas seria encontrada no arquivo para o qual você redirecionou stderr.

Em cenários que exigem tratamento de erro ou registro mais sofisticado, o Fish não vem com bibliotecas integradas projetadas explicitamente para isso. No entanto, você pode aproveitar ferramentas externas ou escrever funções para ajudar. Por exemplo, criar uma função de registro simples pode parecer assim:

```fish
function log_error
    echo $argv >&2
end

log_error "Esta é uma mensagem de erro avançada"
```

Esta função `log_error` receberá qualquer string que você der a ela e a escreverá em stderr. Usar funções como esta pode ajudar a manter seu tratamento de erro limpo e consistente por todo o seu script.
