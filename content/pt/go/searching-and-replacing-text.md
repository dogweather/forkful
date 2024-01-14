---
title:                "Go: Substituindo e substituindo texto."
simple_title:         "Substituindo e substituindo texto."
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

##Por que

Ao escrever código em qualquer linguagem de programação, é comum precisarmos fazer alterações em partes específicas do texto. Isso pode ser desde correção de erros ortográficos até substituição de variáveis em várias linhas de código. Para facilitar esse processo, é importante saber como pesquisar e substituir textos em seu código.

##Como Fazer

Felizmente, Go possui uma função embutida chamada `Replace` que nos permite realizar essa tarefa de forma rápida e eficiente. Para usar essa função, precisamos seguir alguns passos simples:

1. Defina a variável que deseja fazer a substituição.
```Go
texto := "Bem-vindo ao meu blog em Go!"
```
2. Use a função `Replace` para trocar o texto que você deseja.
```Go
novoTexto := strings.Replace(texto, "Go", "Python", 1)
```
No exemplo acima, a função `Replace` irá substituir a primeira ocorrência da palavra "Go" por "Python" na variável `texto`, resultando em "Bem-vindo ao meu blog em Python!". O último parâmetro na função (`1`) indica o número de substituições que serão feitas.

Você também pode fazer substituições em todo o texto, em vez de apenas a primeira ocorrência, adicionando `-1` como último parâmetro na função.

```Go
novoTexto := strings.Replace(texto, "Go", "Python", -1)
```
##Mergulho Profundo

A função `Replace` também nos permite fornecer uma função de substituição como terceiro parâmetro. Isso significa que podemos fazer alterações mais complexas no texto, usando expressões regulares ou outras lógicas. Aqui está um exemplo de como usar uma função de substituição:
```Go
texto := "Hoje é 15 de março de 2021"

novoTexto := strings.Replace(texto, "15 de março de 2021", func(str string) string {
    return strings.ToUpper(str)
}, 1)
```
Neste exemplo, estamos substituindo o texto "15 de março de 2021" pelo mesmo texto, mas em letras maiúsculas. Isso é possível graças à função de substituição que definimos. Podemos usar esse método para fazer várias alterações complexas em nossos textos.

##Veja Também

- [Documentação oficial do pacote strings em Go](https://golang.org/pkg/strings/)
- [Tutorial de expressões regulares em Go](https://medium.com/@matryer/introducing-regular-expressions-in-go-9d84a571c92f)
- [Guia para iniciantes em Go](https://larien.gitbook.io/aprenda-go-com-testes/)

Agora que você sabe como pesquisar e substituir textos em Go, use essa função para tornar seu código mais eficiente e legível. Compartilhe suas experiências nos comentários!