---
title:                "Escrevendo no erro padrão"
date:                  2024-01-19
html_title:           "Arduino: Escrevendo no erro padrão"
simple_title:         "Escrevendo no erro padrão"

category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## O Que é & Por Que?
Escrever no standard error (stderr) é mandar mensagens de erro para um canal de saída específico, separado da saída normal (stdout). Programadores fazem isso para diferenciar a saída comum dos erros e para poder redirecionar e manipular essas mensagens de forma independente.

## Como Fazer:
```Bash
# Escrevendo mensagem de erro para stderr
echo "Erro: Algo deu errado." >&2

# Exemplo de redirecionamento de stderr para um arquivo
echo "Erro: Falha na operação." >&2 2>erro.log

# Exemplo de redirecionamento de stdout e stderr para diferentes arquivos
echo "Informação normal." >saida.log
echo "Erro grave." >&2 2>erro.log
```

Nos exemplos acima, as mensagens de erro serão exibidas na tela ou salvas no arquivo `erro.log`, dependendo do redirecionamento.

## Deep Dive
Historicamente, a distinção entre stdout e stderr surgiu em sistemas Unix para ajudar na depuração de programas, permitindo que as mensagens de erro fossem tratadas separadamente. Alternativas para escrever em stderr incluem usar comandos e linguagens de programação que ofereçam manipulação embutida de erro, como `stderr.write()` em Python. No Bash, stderr é representado pelo descritor de arquivo 2, enquanto stdout é o descritor 1, daí o uso de `2>` para redirecionar erros. 

## Veja Também:
- Guia sobre redirecionamentos no Bash: https://www.gnu.org/software/bash/manual/bash.html#Redirections
- Explicações detalhadas sobre stdout e stderr: https://www.tldp.org/LDP/abs/html/io-redirection.html
- Práticas recomendadas para manipulação de erros em scripts: https://google.github.io/styleguide/shellguide.html#s7.7-separate-stdout-from-stderr
