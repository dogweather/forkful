---
title:    "PHP: Criando um arquivo temporário"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Por que criar um arquivo temporário em PHP?

Criar um arquivo temporário é uma forma eficiente de armazenar dados temporários em um programa PHP. Isso pode ser especialmente útil em cenários onde é necessário salvar informações temporárias, mas não é necessário mantê-las permanentemente.

## Como criar um arquivo temporário em PHP

Para criar um arquivo temporário em PHP, usamos a função `tmpfile()`, que retorna um ponteiro para um novo arquivo temporário. Podemos, então, ler e escrever dados neste arquivo da mesma forma que fazemos com outros arquivos. Veja um exemplo abaixo:

```PHP
// Cria um arquivo temporário
$file = tmpfile();

// Escreve dados no arquivo temporário
fwrite($file, "Este é um exemplo de conteúdo para o arquivo temporário.");
```

Podemos também recuperar o conteúdo do arquivo temporário usando a função `fgets()`, como mostrado no exemplo abaixo:

```PHP
// Volta para o início do arquivo temporário
rewind($file);

// Lê o conteúdo do arquivo temporário
$conteudo = fgets($file);

// Imprime o conteúdo na tela
echo $conteudo; // Output: Este é um exemplo de conteúdo para o arquivo temporário.
```

## Aprofundando no assunto: criando um arquivo temporário seguro

Criar um arquivo temporário é uma tarefa simples, mas é importante garantir que o arquivo seja seguro para uso em seu programa. Para isso, podemos usar a função `sys_get_temp_dir()` para recuperar o diretório temporário do sistema operacional e, em seguida, especificar esse diretório como o local para o nosso arquivo temporário, como mostrado no exemplo abaixo:

```PHP
// Cria um arquivo temporário no diretório temporário do sistema
$file = tmpfile(sys_get_temp_dir());

// Escreve dados no arquivo temporário
fwrite($file, "Este é um exemplo de conteúdo para o arquivo temporário.");

// Fecha o arquivo para garantir que seja excluído
fclose($file);
```

Ao fechar o arquivo, ele será excluído automaticamente, evitando possíveis brechas de segurança em seu programa. É importante lembrar que, ao usar essa técnica, é recomendado especificar um prefixo único para o nome do arquivo temporário, usando a função `tempnam()`.

## Veja também

- [Documentação oficial do PHP sobre a função tmpfile()](https://www.php.net/manual/pt_BR/function.tmpfile.php)
- [Artigo sobre segurança em criação de arquivos temporários em PHP (em inglês)](https://paragonie.com/blog/2016/12/generating-secure-random-numbers-in-php)