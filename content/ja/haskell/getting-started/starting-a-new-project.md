---
date: 2024-01-20 18:03:37.754740-07:00
description: "\u65B0\u3057\u3044\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u3092\u59CB\u3081\
  \u308B\u306E\u306F\u767D\u7D19\u304B\u3089\u30BD\u30D5\u30C8\u30A6\u30A7\u30A2\u3092\
  \u4F5C\u308A\u51FA\u3059\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u306F\u65B0\u305F\u306A\u554F\u984C\u3092\u89E3\u6C7A\u3057\u305F\u308A\u3001\u30A2\
  \u30A4\u30C7\u30A3\u30A2\u3092\u5F62\u306B\u3057\u305F\u308A\u3059\u308B\u305F\u3081\
  \u306B\u65B0\u3057\u3044\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u3092\u59CB\u3081\u307E\
  \u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:01.329398
model: gpt-4-1106-preview
summary: "\u65B0\u3057\u3044\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u3092\u59CB\u3081\
  \u308B\u306E\u306F\u767D\u7D19\u304B\u3089\u30BD\u30D5\u30C8\u30A6\u30A7\u30A2\u3092\
  \u4F5C\u308A\u51FA\u3059\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u306F\u65B0\u305F\u306A\u554F\u984C\u3092\u89E3\u6C7A\u3057\u305F\u308A\u3001\u30A2\
  \u30A4\u30C7\u30A3\u30A2\u3092\u5F62\u306B\u3057\u305F\u308A\u3059\u308B\u305F\u3081\
  \u306B\u65B0\u3057\u3044\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u3092\u59CB\u3081\u307E\
  \u3059\u3002"
title: "\u65B0\u3057\u3044\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u3092\u59CB\u3081\u308B"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
新しいプロジェクトを始めるのは白紙からソフトウェアを作り出すことです。プログラマは新たな問題を解決したり、アイディアを形にしたりするために新しいプロジェクトを始めます。

## How to: (方法)
Haskellで新しいプロジェクトを始めるには、以下の手順を実行します。

```Haskell
-- stackを使用して新しいプロジェクトを作成する
stack new myproject

cd myproject
tree
```
出力例:
```plaintext
myproject/
├── app
│   └── Main.hs
├── src
│   └── Lib.hs
├── test
│   └── Spec.hs
├── myproject.cabal
├── stack.yaml
└── ...
```

プロジェクトの基本的な構造が作られたので、`Main.hs`や`Lib.hs`にコードを書き始めることができます。

## Deep Dive (深堀り)
Haskellは1990年に発表された純粋関数型プログラミング言語です。プロジェクトを始めるのには`cabal`と`stack`という二つのツールがあります。`stack`は設定が簡単で、依存関係管理が行き届いているため、特に新規プロジェクトにおすすめです。`cabal`も強力ですが、より高度な設定やHaskellの経験が必要になることもあります。

Haskellの新プロジェクトでは、プロジェクトのスケルトンを確立し、モジュールやテストのためのディレクトリとファイルを生成します。これは、開発を始める前に一貫性のある構造を与え、保守管理を容易にするためです。

## See Also (関連情報)
以下のリンクから、Haskellのプロジェクト開始に関するさらに多くの情報を見ることができます。

- Haskell公式サイト: [https://www.haskell.org/](https://www.haskell.org/)
- Stackのドキュメント: [https://docs.haskellstack.org/](https://docs.haskellstack.org/)
- Cabalユーザーガイド: [https://www.haskell.org/cabal/users-guide/](https://www.haskell.org/cabal/users-guide/)

これらのソースを参照して、Haskellでのプロジェクト開始に関してより深く学ぶことができます。
