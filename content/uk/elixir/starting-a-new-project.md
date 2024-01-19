---
title:                "Початок нового проєкту"
html_title:           "Elm: Початок нового проєкту"
simple_title:         "Початок нового проєкту"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Що та навіщо?

Запуск нового проекту в Elixir - це ініціювання першого каменя будови вашого програмного рішення. Програмісти роблять це, щоб почати з чистого аркуша, встановити необхідну структуру проекту та визначити залежності.

## Як до цього приступити:

Розпочнемо з встановлення Elixir на вашу систему, якщо цього ще не зроблено. 

```Elixir
sudo apt-get install elixir
```

Далі, запустімо новий проект з допомогою mix, інструменту побудованого непосередньо в Elixir:

```Elixir
mix new my_project
```

Готово! Ви створили новий проект з ім'ям my_project. Ось що вам скаже Elixir:

```Elixir
* creating README.md
* creating .gitignore
* creating mix.exs
* creating config
* creating config/config.exs
* creating lib
* creating lib/my_project.ex

Your Mix project was created successfully.
You can use "mix" to compile it, test it, and more:

    cd my_project
    mix test

Run "mix help" for more commands.
```

## Занурення в тему:

Elixir є декларативною, функціональною мовою програмування, спроектована для створення розподіленої та обчислювальної системи. Вона була створена в 2011 році Хосе Валіму, колишнім важливим співробітником в Ruby on Rails.

При написанні коду на Elixir можна скористатися альтернативами старту нового проекту, такими як Rebar3 для Erlang та Leiningen для Clojure. Вони мають подібну до Mix функціональність, але оскільки Mix є нативним для Elixir, він гарантує кращу сумісність та виконання.

Щодо деталей реалізації, Mix генерує для вас схему каталогів, що випливає з Elixir. Він створює файл конфігурації (config.exs), файл ваших основних залежностей проекту (mix.exs), каталог для вашого вихідного коду (lib) та тестів (test).

## Дивись ще:

Щоб отримати більше інформації:

- Офіційна документація Elixir: <https://elixir-lang.org/docs.html>
- Вступний курс Elixir: <https://elixirschool.com/en/>
- Гайд по mix: <https://hexdocs.pm/mix/Mix.html>