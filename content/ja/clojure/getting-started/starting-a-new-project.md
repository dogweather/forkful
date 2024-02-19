---
aliases:
- /ja/clojure/starting-a-new-project/
date: 2024-01-20 18:03:09.223830-07:00
description: "\u65B0\u3057\u3044\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u3092\u59CB\u3081\
  \u308B\u3063\u3066\u306E\u306F\u3001\u30A2\u30A4\u30C7\u30A2\u3092\u30B3\u30FC\u30C9\
  \u306B\u5909\u3048\u3066\u4F5C\u54C1\u3092\u4F5C\u308B\u30B9\u30BF\u30FC\u30C8\u5730\
  \u70B9\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u306F\u3001\u65B0\u3057\u3044\
  \u554F\u984C\u3092\u89E3\u6C7A\u3057\u305F\u308A\u3001\u5B66\u3093\u3060\u308A\u3001\
  \u4F5C\u308A\u305F\u3044\u3082\u306E\u3092\u5F62\u306B\u3059\u308B\u305F\u3081\u306B\
  \u65B0\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u3092\u7ACB\u3061\u4E0A\u3052\u307E\u3059\
  \u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:54.604429
model: gpt-4-1106-preview
summary: "\u65B0\u3057\u3044\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u3092\u59CB\u3081\
  \u308B\u3063\u3066\u306E\u306F\u3001\u30A2\u30A4\u30C7\u30A2\u3092\u30B3\u30FC\u30C9\
  \u306B\u5909\u3048\u3066\u4F5C\u54C1\u3092\u4F5C\u308B\u30B9\u30BF\u30FC\u30C8\u5730\
  \u70B9\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u306F\u3001\u65B0\u3057\u3044\
  \u554F\u984C\u3092\u89E3\u6C7A\u3057\u305F\u308A\u3001\u5B66\u3093\u3060\u308A\u3001\
  \u4F5C\u308A\u305F\u3044\u3082\u306E\u3092\u5F62\u306B\u3059\u308B\u305F\u3081\u306B\
  \u65B0\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u3092\u7ACB\u3061\u4E0A\u3052\u307E\u3059\
  \u3002"
title: "\u65B0\u3057\u3044\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u3092\u59CB\u3081\u308B"
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
