---
title:                "C: Baixando uma página da web"
simple_title:         "Baixando uma página da web"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Por que

Não há dúvida de que a internet é uma parte essencial de nossas vidas hoje em dia. E com a abundância de informações disponíveis online, muitas vezes encontramos páginas da web que nos interessam e queremos tê-las em nossos dispositivos para acesso mais rápido ou até mesmo para uso offline. É aí que entra o código em C para download de páginas da web, permitindo que você salve o conteúdo de uma página diretamente em seu dispositivo.

## Como Fazer

Para começar, vamos criar um novo projeto em C e importar a biblioteca padrão `stdio.h` para que possamos trabalhar com entrada e saída de dados. A primeira etapa é definir a URL da página que desejamos baixar em uma variável.

````C
#include <stdio.h>

int main(){
    char url[] = "https://www.example.com"; // Substitua com seu URL desejado
    // Resto do código aqui
}
````

Agora, para baixar a página da web, precisamos usar a função `fopen()` para abrir uma conexão com a URL especificada e, em seguida, usar a função `fputc()` para gravar o conteúdo HTML da página em um arquivo que criaremos usando `fopen()`. Lembre-se de iniciar o loop enquanto houver conteúdo HTML a ser baixado.

````C
FILE *arquivo;
arquivo = fopen("pagina.html", "w"); // Criando um arquivo chamado pagina.html

fputc('<', arquivo);

int c;
while((c = fgetc(url)) != EOF){
    fputc(c, arquivo);
}

fputc('>', arquivo);
fclose(arquivo);
````

Agora, se você executar o código, verá que o conteúdo HTML completo da página especificada será salvo em um arquivo chamado `pagina.html`.

## Mergulho Profundo

Para aprofundar ainda mais, você pode explorar outras funções de entrada e saída de dados, como `fprintf()`, `fgets()`, `fprintf()`, etc. Essas funções podem ajudar a tornar seu código mais eficiente e também oferecem mais opções de gravação e leitura de dados. Você também pode adicionar recursos como nomear o arquivo com base no título da página ou usando o URL da página como nome do arquivo. Além disso, você pode explorar bibliotecas externas para baixar páginas mais complexas com conteúdo dinâmico.

## Veja Também 

- [Biblioteca de funções padrão do C](https://pt.wikipedia.org/wiki/C_(linguagem_de_programa%C3%A7%C3%A3o))
- [Documentação do C na linguagem portuguesa](https://www.gnu.org/software/gnu-c-manual/gnu-c-manual-2.0.0.pdf)
- [Tutorial de entrada e saída em C](https://www.programiz.com/c-programming/c-file-input-output)