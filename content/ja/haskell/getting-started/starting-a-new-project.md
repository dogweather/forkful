---
date: 2024-01-20 18:03:37.754740-07:00
description: "How to: (\u65B9\u6CD5) Haskell\u3067\u65B0\u3057\u3044\u30D7\u30ED\u30B8\
  \u30A7\u30AF\u30C8\u3092\u59CB\u3081\u308B\u306B\u306F\u3001\u4EE5\u4E0B\u306E\u624B\
  \u9806\u3092\u5B9F\u884C\u3057\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.185010-06:00'
model: gpt-4-1106-preview
summary: "Haskell\u3067\u65B0\u3057\u3044\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u3092\
  \u59CB\u3081\u308B\u306B\u306F\u3001\u4EE5\u4E0B\u306E\u624B\u9806\u3092\u5B9F\u884C\
  \u3057\u307E\u3059."
title: "\u65B0\u3057\u3044\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u3092\u59CB\u3081\u308B"
weight: 1
---

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
