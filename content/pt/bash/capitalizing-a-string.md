---
title:                "Bash: Capitalizando uma cadeia de caracteres."
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que usar Bash para capitalizar uma string?

Muitos usuários do Bash podem se deparar com a necessidade de capitalizar uma string durante a programação. Isso pode ser útil para ajustar a formatação de textos ou para fins de validação de dados. Felizmente, o Bash possui uma maneira simples de realizar essa tarefa.

## Como fazer

Para capitalizar uma string em Bash, podemos usar o comando `tr` (traduzir) junto com algumas opções. Por exemplo, se quisermos capitalizar todas as letras de uma string, podemos usar o seguinte comando:

```
Bash
echo "exemplo de string" | tr '[:lower:]' '[:upper:]'
```

Isso irá retornar a string "EXEMPLO DE STRING". Podemos também usar o comando `sed` para capitalizar a primeira letra de uma string:

```
Bash
echo "exemplo de string" | sed 's/.*/\L&t/; s/^[[:lower:]]/\u&/'
```

Este comando primeiro transformará todas as letras em minúsculas e depois usará a opção `\u` para capitalizar a primeira letra.

## Deep Dive

O comando `tr` usa o conceito de tradução de caracteres para capitalizar as letras em uma string. Ele troca as letras minúsculas pela versão maiúscula correspondente, conforme definido pelas opções especificadas. Já o comando `sed` usa expressões regulares para substituir a primeira letra de uma string por sua versão maiúscula.

É importante ressaltar também que esses comandos são sensíveis à configuração de idioma do sistema. Por exemplo, em alguns idiomas, a letra "i" pode ser maiúscula e em outros, pode ser minúscula.

## Veja também

- Documentação oficial do Bash: https://www.gnu.org/software/bash/
- Tutoriais de Bash para iniciantes: https://linuxize.com/tags/bash/
- Perguntas frequentes sobre Bash: https://www.linuxjournal.com/content/bash-faqs