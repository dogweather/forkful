---
title:    "Clojure: Traduzindo: Converting a date into a string"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

##Por que

Se você é um programador iniciante em Clojure ou apenas quer aprender uma nova habilidade, converter uma data em uma string pode ser um desafio interessante. Além disso, pode ser uma tarefa necessária em muitos projetos e é importante dominá-la para se tornar um programador mais completo.

##Como Fazer

Converter uma data em uma string em Clojure é bastante simples e pode ser feito usando a função `str`. Veja um exemplo abaixo:

```Clojure
(str "A data de hoje é " (java.util.Date.))
```

Esse código irá retornar uma string contendo a frase "A data de hoje é" seguida da data atual. O resultado será algo como "A data de hoje é Thu Jan 07 10:15:34 BRT 2021".

Outra forma de obter uma string com a data atual é usando a função `format` em conjunto com a classe `java.time.LocalDate`, assim:

```Clojure
(format (java.time.LocalDate/now) "A data de hoje é %1$td/%1$tm/%1$tY")
```

Ou, se você preferir uma saída em formato brasileiro, use:

```Clojure
(format (java.time.LocalDate/now) "A data de hoje é %1$td/%1$tm/%1$tY" (java.util.Locale./getBrazil()))
```

O resultado será algo como "A data de hoje é 07/01/2021" ou "A data de hoje é 07/01/2021" dependendo da sua escolha.

##Mergulho Profundo

Depois de entender como converter uma data em uma string simples, é importante aprender a formatar essa string de acordo com suas necessidades específicas. Para isso, é necessário usar a função `format` em conjunto com o objeto `java.time.format.DateTimeFormatter`. O `DateTimeFormatter` permite que você personalize a saída da string de data, adicionando símbolos e letras especiais. Aqui está um exemplo de como usar o `DateTimeFormatter`:

```Clojure
(format (java.time.LocalDate/now) (java.time.format.DateTimeFormatter/ofPattern "dd 'de' MMMM 'de' yyyy"))
```

Com esse código, o resultado será algo como "07 de janeiro de 2021".

##Veja Também

- Documentação oficial do Clojure para a função [`str`](https://clojure.org/guides/api_strings)
- Tutorial em vídeo sobre como converter uma data em uma string em Clojure: [https://www.youtube.com/watch?v=_jJlliuvqtM](https://www.youtube.com/watch?v=_jJlliuvqtM)