---
title:                "Verificando se um diretório existe"
aliases:
- /pt/bash/checking-if-a-directory-exists/
date:                  2024-02-03T19:06:42.507057-07:00
model:                 gpt-4-0125-preview
simple_title:         "Verificando se um diretório existe"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O que & Por quê?

Na programação Bash, verificar se um diretório existe é um mecanismo de controle essencial usado para verificar a presença de um diretório antes de realizar operações de arquivo. Essa verificação é crucial para evitar erros, como tentar acessar ou modificar diretórios que não existem, garantindo a execução de scripts mais suave e previsível.

## Como fazer:

No seu núcleo, Bash permite verificar a existência de um diretório usando declarações condicionais e o operador `-d`. Abaixo está um exemplo simples que demonstra como realizar essa verificação.

```bash
if [ -d "/caminho/para/diretorio" ]; then
    echo "O diretório existe."
else
    echo "O diretório não existe."
fi
```

Saída de exemplo (se o diretório existir):
```
O diretório existe.
```

Saída de exemplo (se o diretório não existir):
```
O diretório não existe.
```

Para scripts mais complexos, é comum combinar a verificação com outras operações, como criar o diretório se ele não existir:

```bash
DIR="/caminho/para/diretorio"
if [ -d "$DIR" ]; then
    echo "$DIR existe."
else
    echo "$DIR não existe. Criando agora..."
    mkdir -p "$DIR"
    echo "$DIR criado."
fi
```

Saída de exemplo (se o diretório não existir e depois for criado):
```
/caminho/para/diretorio não existe. Criando agora...
/caminho/para/diretorio criado.
```

Embora o próprio Bash forneça ferramentas robustas para essas verificações, não existem bibliotecas de terceiros populares especificamente para esta tarefa, já que comandos Bash nativos são totalmente capazes e eficientes para a validação da presença de diretórios.
