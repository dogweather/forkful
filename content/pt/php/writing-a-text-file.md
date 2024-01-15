---
title:                "Escrevendo um arquivo de texto"
html_title:           "PHP: Escrevendo um arquivo de texto"
simple_title:         "Escrevendo um arquivo de texto"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/writing-a-text-file.md"
---

{{< edit_this_page >}}

Por que escrever um arquivo de texto em PHP?

Existem muitas razões pelas quais alguém pode querer escrever um arquivo de texto em PHP. Pode ser necessário armazenar informações de forma persistente, criar arquivos de log para rastrear erros em um aplicativo web, gerar relatórios ou até mesmo criar um arquivo de configuração para um projeto.

Como fazer:

Para escrever um arquivo de texto em PHP, você precisará seguir alguns passos simples:

1. Abra um arquivo usando a função `fopen()` com o modo "w", que indica que o arquivo será aberto para escrita.

```PHP
$file = fopen("arquivo.txt", "w");
```
2. Em seguida, use a função `fwrite()`, que recebe dois parâmetros: o ponteiro do arquivo e a string que você deseja escrever. Lembre-se de adicionar "\n" ao final da string para pular para uma nova linha.

```
fwrite($file, "Hello World!\n");
```
3. Repita o passo 2 quantas vezes necessário para escrever todas as informações desejadas.

4. Finalmente, use a função `fclose()` para fechar o arquivo.

```PHP
fclose($file);
```

Exemplo completo:

```PHP
$file = fopen("arquivo.txt", "w");
fwrite($file, "Hello World!\n");
fwrite($file, "Este é um exemplo de escrita em arquivo usando PHP.\n");
fclose($file);
```

O arquivo resultante terá o seguinte conteúdo:

```
Hello World!
Este é um exemplo de escrita em arquivo usando PHP.
```

Profundando...

Além dos modos "w" (escrita) e "r" (leitura), a função `fopen()` também possui outros modos interessantes para escrever em arquivos, como "a" (anexar), que adiciona informações ao final do arquivo sem substituir o conteúdo existente, ou "x" (exclusivo), que cria um novo arquivo, mas retorna um erro se o arquivo já existir.

É importante lembrar de sempre fechar o arquivo com `fclose()` após terminar de escrever, pois isso libera recursos do sistema e evita problemas de permissão.

Veja também:

- [Documentação oficial sobre `fopen()` ](https://www.php.net/manual/en/function.fopen.php)
- [Tutorial sobre escrita em arquivos com PHP](https://www.php.net/manual/en/function.fwrite.php)
- [Diferentes modos de abertura de arquivos](https://www.w3schools.com/php/func_filesystem_fopen.asp)

Enfim, agora você já sabe como escrever um arquivo de texto em PHP! Use esses conhecimentos para deixar seus aplicativos ainda mais poderosos.