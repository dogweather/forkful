---
title:                "Verificando se um diretório existe"
html_title:           "Javascript: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## O que e por que?
Verificar se um diretório existe é um processo usado por programadores para confirmar se um diretório específico existe em um sistema de arquivos. Isso é importante porque ajuda a evitar erros e falhas no código, garantindo que o diretório necessário esteja presente antes de executar certas tarefas.

## Como fazer:
`` `javascript
if (fs.existsSync(dirPath)) {
  console.log('O diretório existe!');
} else {
  console.log('O diretório não existe!');
}
`` `
**Saída**: Se o diretório existir, o console exibirá "O diretório existe!". Se não existir, o console exibirá "O diretório não existe!".

## Mergulho profundo:
**1) Contexto histórico:** A função fs.existsSync() foi introduzida no Node.js versão 8.5.0 e foi inspirada pela função fs.exists() presente no módulo "fs" do Node.js versão 0.1.22 e posterior. Antes dessas funções, os programadores precisavam usar a função fs.stat() para verificar a existência de um diretório.

**2) Alternativas**: Além do Node.js, existem outras maneiras de verificar se um diretório existe em diferentes linguagens de programação, como o comando "test -d" do Bash e a função os.path.exists() do Python.

**3) Detalhes de implementação**: A função fs.existsSync() retorna um valor booleano (verdadeiro ou falso) que indica se o diretório existe ou não. Se houver um erro ao verificar a existência do diretório, ele lançará uma exceção.

## Veja também:
- Documentação oficial do Node.js sobre fs.existsSync(): https://nodejs.org/api/fs.html#fs_fs_existssync_path
- Outras maneiras de verificar se um diretório existe em diferentes linguagens de programação: https://www.geeksforgeeks.org/how-to-check-if-a-directory-or-a-file-exists-in-system/
- Como usar a função fs.existsSync() na prática: https://www.digitalocean.com/community/tutorials/how-to-use-node-js-to-check-if-a-file-or-directory-exists