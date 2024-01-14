---
title:                "Haskell: 新しいプロジェクトを始める"
programming_language: "Haskell"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## なぜ

プログラミングの世界では常に新しいプロジェクトが生まれています。新しいプロジェクトを始める理由は人それぞれでしょう。しかし、新しいプロジェクトを始めることで、新しいアイデアを実現したり、スキルを磨いたり、社会に貢献することができるかもしれません。

## はじめに

Haskellは純粋関数型プログラミング言語の一つであり、強力な型システムを持っています。今回は、Haskellを使って新しいプロジェクトを始める方法を紹介します。

まずは、Haskellのコードを書くために必要な環境を整えましょう。Haskellの開発環境をインストールするためには、[Haskell Platform](https://www.haskell.org/platform/)をダウンロードしてインストールする必要があります。また、[Atom](https://atom.io/)や[Visual Studio Code](https://code.visualstudio.com/)などのエディターを使用することをおすすめします。

プロジェクトを始める前に、まずはプロジェクトの目的を明確にしましょう。どのような問題を解決するのか、どのような機能を実装するのか、どのようなユーザーを対象にするのか、などを考えることが重要です。目的が明確になれば、コードを書く際の方針も定まりやすくなります。

## ディープダイブ

新しいプロジェクトを始める際、まずはルートディレクトリーを作成し、その中に```src```と```test```のディレクトリーを作成しましょう。```src```ディレクトリーには、プロジェクトのコードを格納し、```test```ディレクトリーにはテストコードを格納します。

次に、プロジェクトの依存関係を管理するために、[Cabal](https://www.haskell.org/cabal/)を使用します。```cabal init```コマンドを実行すると、プロジェクト用のCabalパッケージが作成されます。このパッケージには、プロジェクトの設定や依存関係などが記述されています。

さらに、Haskellのコードを書く際には、```ghci```コマンドを使用してREPLを起動することもできます。REPLを使用すると、コードを実行しながら試行錯誤することができます。

最後に、Haskellには豊富なライブラリーが用意されていますので、必要な機能を実装する際には、事前に調べて使いやすいライブラリーがあれば積極的に活用するようにしましょう。

## See Also

- [Haskell公式サイト](https://www.haskell.org/)
- [Haskell入門サイト](https://www.learnyouahaskell.com/)
- [Real World Haskell](http://book.realworldhaskell.org/)
- [Haskell Reddit](https://www.reddit.com/r/haskell_jp/)