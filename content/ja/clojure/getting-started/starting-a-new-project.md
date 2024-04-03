---
date: 2024-01-20 18:03:09.223830-07:00
description: "How to: (\u3084\u308A\u65B9) Clojure\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\
  \u3092\u59CB\u3081\u308B\u306E\u306F\u3001Leiningen\u3084Boot\u3001Clojure CLI\u306E\
  \u30C4\u30FC\u30EB\u3092\u4F7F\u3063\u3066\u3067\u3059\u3002\u3053\u3053\u3067\u306F\
  \u3001Clojure CLI\u3092\u4F8B\u306B\u3057\u3066\u307F\u307E\u3057\u3087\u3046\u3002\
  \u30BF\u30FC\u30DF\u30CA\u30EB\u3092\u958B\u3044\u3066\u3001\u4EE5\u4E0B\u306E\u30B3\
  \u30DE\u30F3\u30C9\u3092\u6253\u3063\u3066\u307F\u3066\u304F\u3060\u3055\u3044\uFF1A\
  ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:41.559000-06:00'
model: gpt-4-1106-preview
summary: "Clojure\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u3092\u59CB\u3081\u308B\u306E\
  \u306F\u3001Leiningen\u3084Boot\u3001Clojure CLI\u306E\u30C4\u30FC\u30EB\u3092\u4F7F\
  \u3063\u3066\u3067\u3059\u3002\u3053\u3053\u3067\u306F\u3001Clojure CLI\u3092\u4F8B\
  \u306B\u3057\u3066\u307F\u307E\u3057\u3087\u3046\u3002\u30BF\u30FC\u30DF\u30CA\u30EB\
  \u3092\u958B\u3044\u3066\u3001\u4EE5\u4E0B\u306E\u30B3\u30DE\u30F3\u30C9\u3092\u6253\
  \u3063\u3066\u307F\u3066\u304F\u3060\u3055\u3044\uFF1A."
title: "\u65B0\u3057\u3044\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u3092\u59CB\u3081\u308B"
weight: 1
---

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
