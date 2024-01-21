---
title:                "Criando um arquivo temporário"
date:                  2024-01-20T17:40:12.390273-07:00
model:                 gpt-4-1106-preview
simple_title:         "Criando um arquivo temporário"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why?

Criar um arquivo temporário é o processo de geração de um arquivo que é projetado para ser utilizado por um curto período e removido depois. Programadores fazem isso quando precisam de um espaço seguro para operar dados sem alterar o estado permanente ou para gerenciar dados que só são necessários durante a execução de um processo específico.

## How to:

Atualmente, Gleam não possui uma biblioteca padrão para manipulação de arquivos diretamente, então vamos pensar fora da caixa e usar uma chamada de sistema para lidar com arquivos temporários. Servindo-nos de uma função do sistema operacional como `mkstemp`, faríamos algo semelhante ao código em um shell script ou em outro idioma de sistema.

Supondo que você esteja em um sistema Unix-like:

```gleam
external fn mkstemp(template: String) -> Int 
  "system.mkstemp"

fn create_temp_file() {
  let template = "tempXXXXXX" // As Xs são substituídas por caracteres aleatórios
  mkstemp(template)
}
```

Saída de exemplo (o número retornado é o descritor do arquivo):

```shell
123456
```

## Deep Dive

A função de sistema `mkstemp` vem desde os tempos do Unix, uma escolha robusta porque lida com as possíveis condições de corrida que podem acontecer ao criar arquivos temporários em um ambiente com múltiplas threads ou processos. Alternativamente, em outros idiomas de programação, pode-se encontrar bibliotecas específicas para gerenciar arquivos temporários, como `tempfile` no Python.

Ao criar um arquivo temporário, o sistema se certifica de que o nome do arquivo é único, evitando sobrescritas e possíveis problemas de segurança. A remoção do arquivo após seu uso é uma boa prática, principalmente em aplicações que criam um volume alto de temporários, para não sobrecarregar o sistema de arquivos.

## See Also

Para mais detalhes sobre a funcionalidade `mkstemp`, confira a documentação da chamada de sistema do UNIX:
- [Man page for mkstemp](https://linux.die.net/man/3/mkstemp)

Para conceitos adicionais sobre manipulação de arquivos em Gleam e funções externas, veja:
- [Gleam documentation on external functions](https://gleam.run/book/tour/external-functions.html)

Note que, à medida que Gleam amadurece, é provável que surjam bibliotecas para simplificar estas tarefas, então fique de olho nos repositórios de pacotes do Gleam:
- [Hex package manager for the Erlang ecosystem](https://hex.pm/)