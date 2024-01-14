---
title:    "TypeScript: Verificando se um diretório existe"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Por que verificar se um diretório existe?

Verificar se um diretório existe é uma tarefa importante em projetos de programação. Isso permite que você garanta que o diretório necessário esteja presente antes de executar determinadas funções. Além disso, pode ajudar a evitar erros e garantir que o código seja mais robusto.

## Como fazer?

Para verificar se um diretório existe em TypeScript, podemos usar a biblioteca nativa 'fs'. Primeiro, importamos a função 'statSync' desta biblioteca, que permite verificar as estatísticas de um determinado arquivo ou diretório. Em seguida, usamos o método 'existsSync' para verificar se o diretório existe ou não.

```TypeScript
import fs from 'fs';
// Verificar se o diretório existe
const dirExists = (path: string) => {
  try {
    fs.statSync(path);
    return true;
  } catch (err) {
    return false;
  }
};
// Exemplo de uso
console.log(dirExists('./meuDiretorio')); // Retorna true se existir, false se não existir
```

## Mergulho Profundo

Ao verificar se um diretório existe, é importante entender que o método 'existsSync' pode retornar 'true' mesmo se o argumento fornecido for um arquivo e não um diretório. Além disso, o diretório em si pode existir, mas pode não estar acessível devido a permissões de arquivo ou disco. Portanto, é recomendado usar o método 'isDirectory' para garantir que o caminho fornecido seja de fato um diretório antes de prosseguir com certas operações no código.

# Veja também

- [Documentação oficial da biblioteca 'fs' em Node.js](https://nodejs.org/api/fs.html)
- [Guia para verificar se um diretório existe em TypeScript](https://www.geeksforgeeks.org/how-to-check-if-a-directory-or-a-file-exists-in-system/)
- [Exemplos práticos de uso do método 'existsSync'](https://www.tutorialspoint.com/nodejs/nodejs_file_system.htm)