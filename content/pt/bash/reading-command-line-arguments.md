---
title:    "Bash: Leitura de argumentos de linha de comando"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Por que ler argumentos de linha de comando?

Se você deseja automatizar tarefas em seu computador, entender como ler argumentos de linha de comando pode ser muito útil. Além disso, muitos programas e scripts usam argumentos de linha de comando para personalizar a execução e o comportamento do programa.

## Como fazer

É muito simples ler argumentos de linha de comando em um script Bash. Basta seguir os passos abaixo:

* Primeiro, use a variável especial `$#` para obter o número total de argumentos passados na linha de comando.

  ```Bash
  #!/bin/bash
  
  echo "Foram passados $# argumentos na linha de comando"
  ```

* Em seguida, use a variável especial `$@` para obter uma lista de todos os argumentos passados, cada um em uma linha separada.

  ```Bash
  #!/bin/bash
  
  for argumento in "$@"
  do
      echo "Argumento: $argumento"
  done
  ```

* Por fim, use a variável especial `$1`, `$2`, etc. para acessar argumentos específicos por sua posição na linha de comando.

  ```Bash
  #!/bin/bash
  
  echo "O primeiro argumento é $1"
  echo "O segundo argumento é $2"
  ```

## Mergulho profundo

Além do básico, podemos fazer mais coisas interessantes com argumentos de linha de comando. Aqui estão algumas dicas e truques:

* Podemos usar a estrutura `case` para verificar argumentos específicos e executar diferentes ações com base nisso.

  ```Bash
  #!/bin/bash
  
  case $1 in
      start) echo "Iniciando o programa...";;
      stop) echo "Parando o programa...";;
      *) echo "Comando inválido. Use 'start' ou 'stop'.";;
  esac
  ```

* Podemos usar a opção `-h` para adicionar um texto de ajuda ao script, tornando-o mais amigável para o usuário.

  ```Bash
  #!/bin/bash
  
  while getopts ":h" opcao; do
      case ${opcao} in
          h) echo "Uso: $0 [-h]";;
          *) echo "Opção inválida. Use '-h' para ajuda.";;
      esac
  done
  ```

* Podemos até mesmo permitir que o usuário insira argumentos personalizados através da opção `-a`.

  ```Bash
  #!/bin/bash
  
  parametro="padrão"
  
  while getopts ":a:" opcao; do
      case ${opcao} in
          a) parametro=$OPTARG;;
          *) echo "Opção inválida. Use '-a valor'.";;
      esac
  done
  
  echo "O valor do parâmetro é $parametro"
  ```

## Veja também

- [Guia de linha de comando do Bash](https://www.devmedia.com.br/guia-de-sintaxe-do-bash/28525)
- [Como utilizar argumentos de linha de comando no Bash](https://www.linuxtechi.com/bash-scripting-using-command-line-arguments/)