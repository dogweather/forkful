---
title:                "新しいプロジェクトを始める"
html_title:           "C: 新しいプロジェクトを始める"
simple_title:         "新しいプロジェクトを始める"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 何で、それなぜ？

新プロジェクトを開始するとは、新しいコードベースをゼロから作り上げることを指します。プログラマはこれを行うことで、特定の問題を解決する独自のソリューションを作成したり、新しいアプリケーションを開発したりして、自分自身を表現し、スキルを向上させる機会を得ることができます。

## 使い方：

新しいGleamプロジェクトを始めるには、以下のコマンドを使用します。サンプルの出力も示します。

```Gleam
$ rebar3 new gleam_lib my_project
```

これにより、新しいプロジェクト「my_project」が作成されます。

## 深堀り：

プロジェクトを始める判断は、プログラマのバックグラウンドや必要とする問題解決策に基づくものです。コーディングは1970年代に出現し、以来、さまざまな方法で進化してきました。初期のプログラマは、しばしば完全なソフトウェアプロジェクトを一人で作成していましたが、現在ではチームが一緒に作業することが一般的です。

Gleamの代わりに、他のErlang VM言語（ElixirやLFEなど）を使用しても新しいプロジェクトを開始することは可能です。しかしGleamには、静的な型システムやErlangとの優れた互換性など、他の言語にはない利点があります。

新プロジェクトを開始するとき、実装詳細に注意を払うことは重要です。良いアーキテクチャ設計は、コードの読みやすさとメンテナンス性を向上させ、バグを減らすことができます。

## 参考資料：

2. [Gleamリポジトリ](https://github.com/gleam-lang/gleam)
4. [Erlang](https://www.erlang.org/)