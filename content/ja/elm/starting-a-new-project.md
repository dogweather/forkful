---
title:                "新しいプロジェクトを始める"
html_title:           "C: 新しいプロジェクトを始める"
simple_title:         "新しいプロジェクトを始める"
programming_language: "Elm"
category:             "Elm"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/starting-a-new-project.md"
---

{{< edit_this_page >}}

**## 何と何故?**

新規プロジェクトを開始するとは、新たなソフトウェア開発を始めることです。私たちプログラマーがこれをする理由は、新たなアイディアを実現し、ユーザーに価値を提供するためです。

**## どうやって:**

```Elm
-- elmをインストールする
npm install -g elm

-- 新規プロジェクトを作成する
elm init
```

これで、新規Elmプロジェクトが作成され、初期設定が自動的に行われます。

**## ディープダイブ**

新規プロジェクトの開始は、Elmプログラミングの大切なステップです。Elmはそのエラーメッセージが非常に親切で、パッケージシステムと型システムが強力なので、初めてのプロジェクトでも容易に始められます。他の選択肢としては、JavaScriptやTypeScriptなどがありますが、Elmはこれらと異なり、開発エクスペリエンスとパフォーマンスを両立させた言語として設計されています。

開始時、Elmの`init`コマンドは項目毎にファイルシステムを設定します。コードは`src`ディレクトリに格納され、Elmのパッケージは`elm-stuff`ディレクトリに保管されます。また、`elm.json`ファイルが作成され、プロジェクトの依存関係と設定情報を管理します。

**## 参照先**

- Elm公式ドキュメンテーション: [https://elm-lang.org/docs](https://elm-lang.org/docs)