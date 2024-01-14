---
title:    "Gleam: Verificando se um diretório existe"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Por que verificar a existência de um diretório em Gleam?

Às vezes, ao escrever um programa em Gleam, precisamos verificar se um determinado diretório já existe antes de executar determinadas tarefas. Isso pode ser útil para garantir que não haja conflitos ou para evitar erros durante a execução do código. Felizmente, a linguagem Gleam facilita essa verificação.

## Como fazer isso em Gleam

Para verificar se um diretório existe em Gleam, podemos usar a função `is_dir` do módulo `gleam/directory`. Ela recebe uma string contendo o caminho do diretório como parâmetro e retorna um valor booleano indicando se o diretório existe ou não. Veja o exemplo abaixo:

```gleam
import gleam/directory

let dir_path = "./meu_diretorio"
let dir_exists = directory.is_dir(dir_path)

if dir_exists {
  // O código para executar se o diretório existir
} else {
  // O código para executar se o diretório não existir
}

println("O diretório existe? ${dir_exists}")
```

Saída:

```
O diretório existe? true
```

## Detalhes sobre a verificação da existência de um diretório

Ao usar a função `is_dir`, é importante entender algumas coisas. Primeiro, ela não verifica se o caminho fornecido é realmente um diretório, apenas se existe um diretório com esse caminho. Além disso, ela pode ser usada tanto com caminhos absolutos quanto relativos. Por fim, é importante lembrar que o caminho deve ser válido e acessível pelo programa.

# Veja também

- Documentação oficial sobre o módulo `gleam/directory`: https://gleam.run/modules/gleam_directory.html
- Outros módulos úteis do Gleam: https://gleam.run/modules/