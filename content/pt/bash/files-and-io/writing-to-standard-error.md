---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:18.919169-07:00
description: "Escrever para o erro padr\xE3o (stderr) no Bash \xE9 sobre direcionar\
  \ mensagens de erro ou qualquer sa\xEDda de diagn\xF3stico importante separadamente\
  \ da sa\xEDda\u2026"
lastmod: '2024-03-13T22:44:46.769046-06:00'
model: gpt-4-0125-preview
summary: "Escrever para o erro padr\xE3o (stderr) no Bash \xE9 sobre direcionar mensagens\
  \ de erro ou qualquer sa\xEDda de diagn\xF3stico importante separadamente da sa\xED\
  da padr\xE3o (stdout)."
title: "Escrevendo para o erro padr\xE3o"
weight: 25
---

## O quê e Por quê?
Escrever para o erro padrão (stderr) no Bash é sobre direcionar mensagens de erro ou qualquer saída de diagnóstico importante separadamente da saída padrão (stdout). Programadores fazem isso para garantir que mensagens de erro possam ser facilmente identificadas, registradas ou até ignoradas, auxiliando nos processos de depuração e registro.

## Como fazer:
No Bash, você usa `>&2` para redirecionar a saída para stderr. Aqui está um exemplo básico:

```bash
echo "Esta é uma mensagem normal"
echo "Esta é uma mensagem de erro" >&2
```

Ao executar este script, ambas as mensagens serão exibidas no console, mas se você redirecioná-las, pode separar o stdout do stderr. Por exemplo:

```bash
bash script.sh > output.txt 2> error.txt
```

`output.txt` conterá `"Esta é uma mensagem normal"`, enquanto `error.txt` capturará `"Esta é uma mensagem de erro"`.

Para um caso de uso prático, considere um script que processa arquivos e relata um erro se um arquivo não existir:

```bash
filename="exemplo.txt"

if [ ! -f "$filename" ]; then
    echo "$filename não existe!" >&2
    exit 1
else
    echo "Processando $filename"
fi
```

Saída de amostra diretamente no console quando `exemplo.txt` não existir:

```
exemplo.txt não existe!
```

Não existem bibliotecas de terceiros diretas no Bash para manipulação de stderr, já que o redirecionamento é nativamente suportado e geralmente suficiente. No entanto, para aplicações complexas, frameworks de registro ou ferramentas de registro externas como `syslog` ou `log4bash` podem ser incorporadas para gerenciar tanto o stdout quanto o stderr de forma mais eficaz.
