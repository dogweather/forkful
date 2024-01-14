---
title:    "Ruby: Encontrando o tamanho de uma sequência de caracteres"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por que?

Se você é novo na programação Ruby, pode ficar se perguntando por que é importante encontrar o comprimento de uma string. A resposta é simples: saber o comprimento de uma string é essencial para muitas tarefas de programação, como validar inputs do usuário, formatar saídas e alocar espaço para armazenamento de dados.

## Como fazer

Em Ruby, há algumas maneiras diferentes de encontrar o comprimento de uma string. Aqui estão duas das formas mais comuns:

```
# Método 1: usando o método length
string = "Olá, mundo!"
puts string.length
#Output: 12

# Método 2: usando contagem de caracteres
count = 0
string.each_char { |c| count += 1 }
puts count
#Output: 12
```

## Deep Dive

Para entender completamente como os métodos length e each_char funcionam, é necessário entender a estrutura de string em Ruby. Em Ruby, uma string é um objeto que contém uma sequência de caracteres. Isso significa que cada letra, espaço e símbolo em uma string é considerado como um único caractere. Quando usamos o método length em uma string, ele nos retorna o número total de caracteres na string. Já o método each_char itera sobre cada caractere da string e executa uma ação especificada. Neste exemplo, estamos incrementando a variável de contagem para cada caractere encontrado.

## Veja também

- [Documentação do Ruby para string](https://www.ruby-doc.org/core-2.7.2/String.html)
- [Tutorial de Ruby para iniciantes](https://www.tutorialspoint.com/ruby/index.htm)
- [Blog de programação em Ruby da Brasil MaisTech](https://brasilmaistech.com.br/blog/programacao-em-ruby/)