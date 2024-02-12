---
title:                "新しいプロジェクトを始める"
aliases: - /ja/haskell/starting-a-new-project.md
date:                  2024-01-20T18:03:37.754740-07:00
model:                 gpt-4-1106-preview
simple_title:         "新しいプロジェクトを始める"

tag:                  "Getting Started"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/starting-a-new-project.md"
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
