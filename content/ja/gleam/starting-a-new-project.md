---
title:                "新しいプロジェクトを始める"
date:                  2024-01-20T18:03:21.793727-07:00
model:                 gpt-4-1106-preview
simple_title:         "新しいプロジェクトを始める"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Getting Started"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
新しいプロジェクトを始めることは、アイデアをコードに変える初歩です。プログラマは新しさを求め、問題解決やスキル向上のために新プロジェクトを始めます。

## How to: (方法)
Gleamプロジェクトを始めるのは簡単。コマンド一つでスタート。

```gleam
// Gleam CLIを使って新しいプロジェクトを作成しましょう。
$ rebar3 new lib your_project_name

// 成功したら、下記のような出力が見えます。
===> Writing your_project_name/src/your_project_name_app.erl
===> Writing your_project_name/src/your_project_name_sup.erl
===> Writing your_project_name/src/your_project_name.erl
...
```

## Deep Dive (深掘り)
Gleamは、型安全な関数型言語であり、ErlangのBEAM仮想マシンの上で動作します。始める前は、Erlang と rebar3 をインストールする必要があります。他の BEAM 言語と比べて、Gleamは静的型付けと Erlang VMのパワーを組み合わせることに注力しています。これにより、並行性や分散システムの構築に優れたプラットフォームを提供しています。

## See Also (参照)
- [Gleam公式サイト](https://gleam.run)
- [Gleam GitHubリポジトリ](https://github.com/gleam-lang/gleam)
- [rebar3ドキュメント](https://www.rebar3.org/docs)
