---
title:                "「新しいプロジェクトを始める」"
html_title:           "Elixir: 「新しいプロジェクトを始める」"
simple_title:         "「新しいプロジェクトを始める」"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/starting-a-new-project.md"
---

{{< edit_this_page >}}

## なぜ

新しいプロジェクトを始める理由は様々です。Elixirはバグを予防し、パフォーマンスを最適化することができる優れたプログラミング言語の1つです。

## 作り方

新しいプロジェクトを始めるには、まずElixirをインストールする必要があります。Elixirの公式サイトからダウンロードできます。インストールが完了したら、以下のコマンドを実行して新しいプロジェクトを作成します。

```Elixir
mix new my_project
```

このコマンドにより、"my_project"という名前の新しいディレクトリが作成され、その中に必要なファイルが作成されます。

次に、以下のコマンドを実行してプロジェクトを実行し、サンプル出力を確認します。

```Elixir
cd my_project
mix run -e "IO.puts 'Hello, world!'"
```

"Hello, world!"というメッセージが出力されたら、プロジェクトが正しく動作していることが確認できます。

## 詳細を掘り下げる

新しいプロジェクトを始めるときには、いくつかの重要なファイルに注意する必要があります。

まず、「mix.exs」ファイルはプロジェクトの依存関係を管理するために使用されます。また、「config/config.exs」ファイルにはプロジェクト全体の設定を記述します。

さらに、プロジェクト内に「lib」ディレクトリがあり、ここにコードを書くことでアプリケーションを作成できます。また、「test」ディレクトリにはテストコードを書くことができます。

## 参考リンク

- [Elixir 公式サイト](https://elixir-lang.org/)
- [Elixir 公式ドキュメント](https://elixir-lang.org/docs.html)
- [Elixir Forum](https://elixirforum.com/)