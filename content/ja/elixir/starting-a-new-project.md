---
title:                "新しいプロジェクトを始める"
html_title:           "C: 新しいプロジェクトを始める"
simple_title:         "新しいプロジェクトを始める"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Elixirによる新規プロジェクトの始め方 : はじめに


## なぜ&何のために?

新規プロジェクトを始めるとは、新たなソフトウェアのアイデアや機能を現実にするためのプロセスを始めることです。プログラマーが新規プロジェクトを始める理由は、新たな解決策を作り出したり、既存のシステムやプロセスを改善したりするためです。

## 使い方 :

新規プロジェクトを始める最初のステップは、Elixirのプロジェクトを作成することです。これには以下のコードをターミナル上で実行します。

```Elixir
mix new my_project
```

これにより、新たに`my_project`という名前のプロジェクトが作成されます。

プロジェクトの中に入り、依存関係を取得するために以下のコマンドを実行します。

```Elixir
cd my_project
mix deps.get
```

次に、プロジェクトのテストを実行します。

```Elixir
mix test
```

最後にプロジェクトを実行します。

```Elixir
mix run
```

これら全てのステップを一度に実行するショートカットもあります。

```Elixir
mix new my_project && cd my_project && mix deps.get && mix test && mix run
```

## 深掘り :

新規プロジェクトの始め方に関しては、歴史的な背景や代替手段、具体的な実装の詳細といったさらなる情報があります。

1. 歴史的な背景: Elixirは元々Erlang VMで動作するための言語として開発され、これにより分散システムの開発に役立つよう設計されています。
    
2. 代替案: 新規プロジェクトを始める他の方法としては、既存のプロジェクトテンプレートを使用することや、プロジェクト生成ツールを使用することがあります。しかし、`mix new`はこれらのすべてを包含した一般的な方法です。

3. 実装の詳細: `mix new` コマンドはElixirの基本的なプロジェクト構造を生成します。具体的には、`lib`ディレクトリと`test`ディレクトリを含む最小限のElixirプロジェクト構造を作成します。

## 参考資料 :

1. [Elixir 公式ドキュメンテーション](https://elixir-lang.org/docs.html)
2. [Elixir School](https://elixirschool.com/jp/)
3. [MixとOTPガイド](https://elixir-lang.jp/getting-started/mix-otp/introduction-to-mix.html)