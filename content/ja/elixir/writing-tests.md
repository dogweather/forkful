---
title:                "テストを書く"
html_title:           "Elixir: テストを書く"
simple_title:         "テストを書く"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## 何が & なんで？
テストの書き方とは、プログラマーがソフトウェアの動作を確認するための手法です。我々がテストをする理由は、ソフトウェアが想定通りに動作するかどうかを確認するためです。また、コードのバグやエラーを早期に発見し、修正することができるようにするためでもあります。

## やり方：
```Elixir
# このようにして、単純なテストを行うことができます
assert 1 + 1 == 2
# もちろん、より複雑なテストを書くこともできます
assert 5 / 2 == 2.5
```

## 深堀り：
テストを書くという概念は、プログラミングの世界においては非常に古くから存在しています。その起源の一つには、ドナルド・クヌースによる「ライティング・コンピュータ・ソフトウェア」という本が挙げられます。テストを行う代替手段としては、デバッガーを使用する方法や、静的コード解析ツールを使用する方法があります。Elixirにおいては、テストを書くためのモジュールとしてExUnitが提供されています。

## 関連情報：
- [Elixir: ExUnit](https://hexdocs.pm/ex_unit/ExUnit.html)