---
title:                "新しいプロジェクトを始める"
date:                  2024-01-20T18:03:09.223830-07:00
model:                 gpt-4-1106-preview
simple_title:         "新しいプロジェクトを始める"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Getting Started"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why? (なぜ & なんのために？)
新しいプロジェクトを始めるってのは、アイデアをコードに変えて作品を作るスタート地点です。プログラマは、新しい問題を解決したり、学んだり、作りたいものを形にするために新プロジェクトを立ち上げます。

## How to: (やり方)
Clojureプロジェクトを始めるのは、LeiningenやBoot、Clojure CLIのツールを使ってです。ここでは、Clojure CLIを例にしてみましょう。ターミナルを開いて、以下のコマンドを打ってみてください：

```Clojure
// 新しいプロジェクトを作成
clojure -M:new app myapp

// 作成されたディレクトリに移動
cd myapp

// REPLを起動
clojure -M:repl
```

出力例（プロジェクト名やパスは違う可能性がある）：

```
Generating a project called myapp based on the 'app' template.
The app has been created in /your-path/myapp
Clojure 1.10.3
user=>
```

## Deep Dive (深掘り)
Clojureとプロジェクト管理ツールの関連は歴史的です。Clojureは2007年にリリースされましたが、プロジェクト管理としてLeiningenが人気を博し、その後Clojure CLIが登場しました。Clojure CLIは、設定ファイルやディレクトリ構造に対するよりシンプルなアプローチを提供します。Leiningenは機能が豊富ですが、設定が煩雑になることがあります。Clojure CLIは`deps.edn`ファイルを使って依存関係を管理し、必要最低限の設定でプロジェクトを始められます。

## See Also (関連情報)
Clojureプロジェクトを始めるためのリソース：

- Clojure公式サイト: [https://clojure.org/](https://clojure.org/)
- Clojure CLIガイド: [https://clojure.org/guides/deps_and_cli](https://clojure.org/guides/deps_and_cli)
- Leiningenのホームページ: [https://leiningen.org/](https://leiningen.org/)
- Bootのドキュメント: [https://boot-clj.com/](https://boot-clj.com/)
