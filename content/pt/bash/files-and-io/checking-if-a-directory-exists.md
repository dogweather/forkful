---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:42.507057-07:00
description: "Como fazer: No seu n\xFAcleo, Bash permite verificar a exist\xEAncia\
  \ de um diret\xF3rio usando declara\xE7\xF5es condicionais e o operador `-d`. Abaixo\
  \ est\xE1 um exemplo\u2026"
lastmod: '2024-03-13T22:44:46.767096-06:00'
model: gpt-4-0125-preview
summary: "No seu n\xFAcleo, Bash permite verificar a exist\xEAncia de um diret\xF3\
  rio usando declara\xE7\xF5es condicionais e o operador `-d`."
title: "Verificando se um diret\xF3rio existe"
weight: 20
---

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
