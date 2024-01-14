---
title:    "Bash: Buscando e substituindo texto"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que fazer a substituição de texto em Bash?

A substituição de texto em Bash é uma habilidade fundamental que todo programador deve ter. Ela permite que você faça alterações em massa em um arquivo de texto ou em uma série de arquivos, economizando tempo e esforço. É uma técnica muito útil para automatizar tarefas repetitivas, como a correção de erros de digitação em um grande número de arquivos ou a atualização de informações em um documento.

## Como fazer a substituição de texto em Bash

Para fazer a substituição de texto em Bash, você pode usar o comando `sed` (Stream Editor). Ele é um programa de linha de comando que permite realizar edições em um arquivo de texto seguindo um padrão específico. Vamos ver um exemplo:

```
# Código Bash para substituir todas as ocorrências de "gato" por "cachorro" em um arquivo de texto chamado "animais.txt"
sed 's/gato/cachorro/g' animais.txt
```

Seu arquivo de texto será alterado e cada instância da palavra "gato" será substituída por "cachorro". Você também pode usar expressões regulares para tornar a substituição mais precisa e abrangente.

## Aprofundando na substituição de texto em Bash

Há muitas opções e recursos avançados no comando `sed` que permitem personalizar ainda mais a substituição de texto em Bash. Por exemplo, você pode usar as flags `-i` para fazer a edição diretamente no arquivo original ou `-r` para usar expressões regulares estendidas. Além disso, com o uso de pipes (`|`), é possível conectar vários comandos em uma única linha para realizar várias substituições em um único arquivo.

## Veja também

- [Guia de Referência do comando sed](https://www.gnu.org/software/sed/manual/sed.html)
- [Aprendendo expressões regulares em Bash](https://www.digitalocean.com/community/tutorials/how-to-use-regular-expressions-in-sed)
- [Introdução ao Bash scripting](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)