---
title:                "PHP: Escrevendo um arquivo de texto"
simple_title:         "Escrevendo um arquivo de texto"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Porque escrever um arquivo de texto

Escrever um arquivo de texto é uma tarefa comum para quem está aprendendo a programar em PHP. É uma forma simples e eficiente de armazenar informações que podem ser facilmente acessadas e manipuladas. Além disso, é uma habilidade essencial para criar aplicações web dinâmicas.

## Como fazer

Para escrever um arquivo de texto em PHP, você precisa seguir estes passos:

1. Abra um arquivo usando a função `fopen()`, passando o nome do arquivo e o modo de escrita como parâmetros.
```
$file = fopen("arquivo.txt", "w");
```

2. Escreva o conteúdo do arquivo usando a função `fwrite()`, passando o ponteiro do arquivo e o conteúdo como parâmetros.
```
fwrite($file, "Este é um texto de exemplo");
```

3. Feche o arquivo usando a função `fclose()` para garantir que todas as alterações sejam salvas.
```
fclose($file);
```

4. Pronto! O arquivo de texto foi criado com sucesso.

## Profundidade

Além dos passos básicos descritos acima, existem outras opções que podem ser usadas para escrever um arquivo de texto em PHP. Por exemplo, é possível adicionar conteúdos adicionais em um arquivo existente sem sobrescrevê-lo usando o modo de escrita `a` ao invés de `w`.

Também é importante mencionar que as permissões do arquivo afetam a capacidade de escrever nele. Certifique-se de que o arquivo tenha permissão de escrita para o usuário correto.

## Veja também

- [Documentação do PHP: fopen()](https://www.php.net/manual/pt_BR/function.fopen.php)
- [Documentação do PHP: fwrite()](https://www.php.net/manual/pt_BR/function.fwrite.php)
- [Documentação do PHP: fclose()](https://www.php.net/manual/pt_BR/function.fclose.php)