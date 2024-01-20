---
title:                "Concatenando strings"
html_title:           "Elixir: Concatenando strings"
simple_title:         "Concatenando strings"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

---

## O quê e Por quê?

Concatenar strings é o processo de juntar duas ou mais sequências de caracteres num único valor. Os programadores fazem isso para criar mensagens, comandos ou qualquer outra coisa que precisa de uma string específica.

---

## Como fazer:

Vamos ver um exemplo básico:

```Bash
# Definindo duas strings
string1="Olá, "
string2="mundo!"

# Concatenando as strings
saudacao="$string1$string2"
echo $saudacao
```

O resultado será:

```Bash
Olá, Mundo!
```

Easy peasy!

---

## Aprofundamento

A habilidade de concatenar strings vem dos primórdios da programação. É essencial para tarefas como criar caminhos de arquivos, comandos complexos e mensagens personalizadas.

Existem algumas alternativas para a concatenação de strings no Bash. Podemos, por exemplo, usar o comando `printf`:

```Bash
printf -v saudacao "%s%s" "$string1" "$string2"
echo $saudacao
```

O resultado será o mesmo. A escolha de uma ou outra forma depende das necessidades do seu projeto e do estilo pessoal de programação.

Sobre a implementação, o Bash simplesmente junta os valores sem adicionar espaços ou qualquer outro caractere entre eles. Isso permite um alto grau de controle sobre o resultado final.

---

## Ver também

- Manual do Bash: [https://www.gnu.org/software/bash/manual/bash.html](https://www.gnu.org/software/bash/manual/bash.html)
- Guia avançado de Bash: [http://tldp.org/LDP/abs/html/](http://tldp.org/LDP/abs/html/)

---

Concluindo, a concatenação de strings é uma tarefa simples e comum no Bash. Entenda a teoria, pratique com exemplos e você a dominará rapidinho!