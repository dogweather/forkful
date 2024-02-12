---
title:                "Escrevendo para o erro padrão"
aliases:
- pt/bash/writing-to-standard-error.md
date:                  2024-02-03T19:32:18.919169-07:00
model:                 gpt-4-0125-preview
simple_title:         "Escrevendo para o erro padrão"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
