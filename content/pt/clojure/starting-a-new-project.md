---
title:    "Clojure: Iniciando um novo projeto"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Por que iniciar um novo projeto?

Iniciar um novo projeto em Clojure pode ser uma ótima escolha para aqueles que desejam desenvolver aplicativos e sistemas robustos, escaláveis e funcionais. Com sua sintaxe concisa e sua forte tipagem, o Clojure oferece um ambiente propício para a criação de soluções de alta qualidade.

# Como fazer:

```Clojure
(defn soma [a b]
  (+ a b))
```

Neste exemplo, criamos uma função chamada `soma` que recebe dois parâmetros e retorna a sua soma. Podemos chamar essa função da seguinte forma:

```Clojure
(soma 5 10) ; retorna 15
```

Além disso, a comunidade Clojure oferece uma ampla gama de bibliotecas e ferramentas que facilitam o processo de desenvolvimento. A seguir, mostraremos alguns passos para iniciar um novo projeto em Clojure:

- Instale o leiningen, uma ferramenta de gerenciamento de projetos Clojure
- Crie uma nova estrutura de projeto usando o comando `lein new template`
- Configure o arquivo `project.clj` para adicionar dependências e outras configurações necessárias
- Crie seus arquivos de código fonte com a extensão `.clj`
- Compile e execute seu projeto usando o comando `lein run`

Com esses passos, você estará pronto para começar a desenvolver seu projeto em Clojure.

# Profundidade:

Antes de iniciar um novo projeto em Clojure, é importante entender os principais conceitos por trás dessa linguagem funcional. Algumas das características mais importantes incluem:

- Imutabilidade: objetos em Clojure são imutáveis ​​por padrão, o que garante uma maior segurança e evita efeitos colaterais.
- Coleções persistentes: em Clojure, as coleções são persistentes e as operações de modificação sempre retornam novas instâncias, garantindo assim uma maior eficiência e segurança.
- Sintaxe de lisp: Clojure é baseado na linguagem lisp, que possui uma sintaxe simples e concisa, facilitando a leitura e o entendimento do código.
- JVM: Clojure é executado na máquina virtual Java, o que permite a integração com outras linguagens e bibliotecas já existentes na plataforma.

Além disso, Clojure possui uma comunidade ativa e engajada, com diversos recursos e materiais disponíveis para ajudar no desenvolvimento de projetos.

# Veja também

- [Site oficial do Clojure](https://www.clojure.org/)
- [Leiningen](https://leiningen.org/)
- [ClojureDocs - Documentação e exemplos de código](https://clojuredocs.org/)
- [Canal #Clojure no Slack](https://clojurians.slack.com/)