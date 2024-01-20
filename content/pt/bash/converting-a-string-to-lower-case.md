---
title:                "Convertendo uma string para minúsculas"
html_title:           "Fish Shell: Convertendo uma string para minúsculas"
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## O Que é e Por Quê?

Converter uma string para minúsculas significa alterar todas as letras maiúsculas de uma string para suas correspondentes minúsculas. Programadores fazem isso para normalizar dados, eliminar a diferenciação entre maiúsculas e minúsculas e facilitar comparações e pesquisas.

## Como Fazer:

Em Bash, existem várias maneiras de converter uma string para minúsculas. Abaixo estão alguns exemplos.

```Bash
# Utilizando o comando tr
text='FELIZ ANIVERSÁRIO'
lower_case_text=$(echo "$text" | tr '[:upper:]' '[:lower:]')
echo $lower_case_text
# Saída: feliz aniversário

# Parametro expansion disponível da versão 4.0+
text="OLÁ MUNDO"
echo "${text,,}"
# Saída: olá mundo
```

## Conhecimento Profundo:
Converter uma string para minúsculas tem sido uma necessidade comum para programadores desde que o conceito de sistematização da linguagem ASCII foi introduzido. Tradicionalmente, é realizado usando funções no nível do sistema ou bibliotecas de alto nível, mas desde a versão 4.0, o Bash também oferece essa funcionalidade built-in.

Existem várias alternativas para converter uma string para minúsculas em Bash, como comandos sed, awk e perl, entre outros. No entanto, a expansão de parâmetro introduzida na versão 4.0 oferece uma abordagem mais elegante e rápida. É importante ressaltar que essas variações existem para funcionar em diferentes plataformas e versões do Bash.

Ao converter uma string para minúsculas, é importante levar em consideração as questões de internacionalização e localização. Em Português por exemplo, a ênfase em caracteres como "Ç" pode não ser traduzida corretamente para minúsculas, a menos que a configuração do idioma esteja correta.

## Veja também:

Para mais informações sobre o Bash e a manipulação de strings, consulte os seguintes links:

- GNU Bash: https://www.gnu.org/software/bash/
- String manipulation in bash: https://tldp.org/LDP/abs/html/string-manipulation.html
- StackOverflow Bash String Lowercase: https://stackoverflow.com/questions/2264428/how-to-convert-a-string-to-lower-case-in-bash