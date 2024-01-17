---
title:                "Verificando se um diretório existe"
html_title:           "PHP: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## O que e por que?
Verificar se um diretório existe é uma maneira útil de garantir que seu código funcione corretamente antes de executar certas ações. Programadores frequentemente verificam se um diretório existe para garantir que seu código não seja interrompido por um erro inesperado.

## Como fazer:
Para verificar se um diretório existe em PHP, você pode usar a função `is_dir ()`. Ele retorna `TRUE` se o diretório existir e `FALSE` se não existir. Aqui está um exemplo de código:

```PHP
if(is_dir("/caminho/do/diretório")) {
  echo "O diretório existe.";
}else {
  echo "O diretório não existe.";
}
```

Saída:
`O diretório existe.`

Você também pode usar a função `file_exists ()` para verificar se um diretório existe, mas é importante notar que ela também pode ser usada para verificar se um arquivo existe. Aqui está um exemplo de código que verifica se uma pasta específica existe:

```PHP
if(file_exists("/caminho/do/diretório") && is_dir("/caminho/do/diretório")) {
  echo "O diretório existe.";
}else {
  echo "O diretório não existe.";
}
```

Saída:
`O diretório existe.`

## Detalhes avançados:
Verificar a existência de um diretório é uma técnica de programação comum e útil. Antes de executar ações em um diretório, é importante garantir que ele exista para evitar erros em seu código. Além das funções `is_dir ()` e `file_exist ()`, você também pode usar a função `file_get_contents ()` para verificar se um diretório existe. No entanto, esta função retornará o conteúdo do diretório se ele existir, o que pode não ser útil em algumas situações.

## Veja também:
Para saber mais sobre a função `is_dir ()` e outras funções de verificação de diretório em PHP, confira a documentação oficial do PHP em https://www.php.net/manual/en/book.filesystem.php.