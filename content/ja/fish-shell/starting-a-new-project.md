---
title:                "新しいプロジェクトを開始する"
html_title:           "Fish Shell: 新しいプロジェクトを開始する"
simple_title:         "新しいプロジェクトを開始する"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## なぜ始めるのか

プログラミング言語の一つであるFish Shellを始めるのは、コマンドライン操作をより便利に、よりスマートに行うためです。あなたのシェル体験を大幅に改善するために、この新しいプロジェクトに取り組みましょう。

## やり方

まず、Fish Shellをインストールします。コードブロック内にコマンドを示します。

```
Fish Shellをインストールする方法：

brew install fish
```

次に、Fish Shellに保存されたコマンドを単一の行で表示する方法を学びましょう。

```
コマンドの表示方法：

bind; funced; fish_mode; funcsave; key_bindings -L
```

最後に、独自の関数を作成して、あなたのプロジェクトを最適化します。

```
独自の関数の作成方法：

function project
  # あなたのプロジェクトに実行するコマンドを記述します
end
```

## 深ぼる

新しいプロジェクトを始める際には、プロジェクト固有の関数やエイリアスを作成することで、より効率的な作業を行うことができます。また、Fish Shellは他のシェルよりも柔軟であり、より多くのカスタマイズが可能です。ぜひ、自分に合ったカスタマイズを探求してみてください。

## 参考リンク

- [Fish Shellの公式サイト](https://fishshell.com/)
- [Fish Shellのドキュメンテーション](https://fishshell.com/docs/current/)
- [Fish Shellを使いこなすためのTips](https://dev.to/jonhoo/tips-for-fish-shell-2mmc)