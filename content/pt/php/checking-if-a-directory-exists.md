---
title:                "Verificando se um diretório existe"
date:                  2024-01-20T14:57:44.758045-07:00
html_title:           "Fish Shell: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Verificar Existência de Diretórios em PHP: Um Guia Descomplicado

## O Que & Por Que?
Verificar se um diretório existe é essencial para prevenir erros. Programadores fazem isso para confirmar que o caminho especificado está acessível antes de tentar ler, escrever ou modificar arquivos.

## Como Fazer:
```PHP
<?php
$diretorio = "/meu/caminho/preferido";

if (is_dir($diretorio)) {
    echo "Opa, o diretório existe!";
} else {
    echo "Vish, o diretório não existe!";
}
?>
```
Saída esperada:
```
Opa, o diretório existe!
```
ou
```
Vish, o diretório não existe!
```

## A Profundidade do Tema
A função `is_dir()` está na caixa de ferramentas do PHP desde os primeiros dias da linguagem. Alternativas incluem `file_exists()`, que também verifica arquivos, mas `is_dir()` é a escolha certa quando você quer especificamente um diretório. A implementação básica verifica a tabela de sistema do arquivo para confirmar a existência do caminho. Vale lembrar a diferença de comportamento em diferentes sistemas operativos: caminhos absolutos no Linux começam com '/', enquanto no Windows usam letras de unidade ('C:', 'D:', etc.). Além disso, cuidado com as permissões de acesso: um diretório pode existir, mas não estar acessível devido a restrições.

## Veja Também
- [PHP Manual - is_dir()](https://www.php.net/manual/pt_BR/function.is-dir.php)
- [PHP Manual - file_exists()](https://www.php.net/manual/pt_BR/function.file-exists.php)
- [w3schools - PHP File Handling](https://www.w3schools.com/php/php_file.asp)
