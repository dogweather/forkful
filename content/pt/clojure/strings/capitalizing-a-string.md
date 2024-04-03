---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:04:47.894004-07:00
description: "Capitalizar uma string envolve modificar a string de modo que seu primeiro\
  \ caractere seja mai\xFAsculo, enquanto o restante da string permanece inalterado.\u2026"
lastmod: '2024-03-13T22:44:46.180601-06:00'
model: gpt-4-0125-preview
summary: "Capitalizar uma string envolve modificar a string de modo que seu primeiro\
  \ caractere seja mai\xFAsculo, enquanto o restante da string permanece inalterado."
title: Capitalizando uma string
weight: 2
---

## O Que & Por Quê?
Capitalizar uma string envolve modificar a string de modo que seu primeiro caractere seja maiúsculo, enquanto o restante da string permanece inalterado. Programadores frequentemente realizam a capitalização de strings para garantir consistência dos dados, especialmente para nomes e lugares ou para cumprir com regras gramaticais em interfaces de usuário.

## Como fazer:
Clojure, sendo uma linguagem JVM, permite que você utilize diretamente os métodos de String do Java. Aqui está um exemplo básico de como capitalizar uma string em Clojure:

```clojure
(defn capitalize-string [s]
  (if (empty? s)
    s
    (str (clojure.string/upper-case (subs s 0 1)) (subs s 1))))

(capitalize-string "olá mundo!") ; => "Olá mundo!"
```

Clojure não inclui uma função embutida especificamente para capitalizar strings, mas como mostrado, você pode facilmente alcançar isso combinando as funções `clojure.string/upper-case`, `subs` e `str`.

Para uma solução mais concisa e para lidar com manipulações de strings mais complexas, você pode recorrer a uma biblioteca de terceiros. Uma biblioteca popular no ecossistema Clojure é `clojure.string`. No entanto, até minha última atualização, ela não oferece uma função `capitalize` direta além do que é demonstrado com as funcionalidades centrais do Clojure, então o método mostrado acima é sua abordagem direta sem a necessidade de adicionar bibliotecas adicionais especificamente para capitalização.

Lembre-se, quando trabalhando com strings em Clojure que interagem com métodos Java, você está efetivamente trabalhando com strings Java, permitindo que você aproveite todo o arsenal de métodos de String do Java diretamente no seu código Clojure, se necessário.
