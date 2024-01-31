---
title:                "Escrevendo um arquivo de texto"
date:                  2024-01-19
simple_title:         "Escrevendo um arquivo de texto"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Escrever um arquivo de texto é registrar dados em um arquivo legível. Programadores fazem isso para salvar configurações, logs ou dados de entrada/saída.

## How to:
Criar e escrever em um arquivo é simples no Bash. Use `echo` ou `printf` junto com o operador `>` para escrever, `>>` para adicionar.

```Bash
echo "Olá, mundo!" > arquivo.txt  # Cria o arquivo.txt com o conteúdo 'Olá, mundo!'
echo "Adiciona linha" >> arquivo.txt  # Adiciona 'Adiciona linha' ao arquivo.txt
printf "Formato %s\n" "específico" >> arquivo.txt  # Adiciona 'Formato específico' usando printf
cat arquivo.txt  # Mostra o conteúdo do arquivo
```

Saída esperada:

```
Olá, mundo!
Adiciona linha
Formato específico
```

## Deep Dive:
Historicamente, a gravação de arquivos em sistemas Unix, como é o Bash, sempre foi feita através de redirecionamento. Por conveniência, ferramentas como `nano`, `vi`, e `emacs` foram criadas para edição direta de texto. `tee` é outra ferramenta que pode tanto mostrar no terminal quanto escrever no arquivo simultaneamente.

Detalhes de implementação incluem cuidados com permissões e manipulação de arquivos em ambientes concorrentes.

## See Also:
- GNU Bash documentation: https://www.gnu.org/software/bash/manual/
- Linux man page for `echo`: https://linux.die.net/man/1/echo
- Advanced Bash-Scripting Guide: https://tldp.org/LDP/abs/html/
