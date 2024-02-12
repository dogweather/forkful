---
title:                "新しいプロジェクトを始める"
aliases:
- /ja/fish-shell/starting-a-new-project.md
date:                  2024-01-20T18:03:46.636467-07:00
model:                 gpt-4-1106-preview
simple_title:         "新しいプロジェクトを始める"

tag:                  "Getting Started"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why? (何とその理由？)
プロジェクトの開始とは、新しいアイデアやソリューションに基づいてコードを書き始めることです。プログラマーは新しい機能を提供、問題を解決、または単に学習と探究を進めるためにこれを行います。

## How to: (方法)
Fish Shellで新しいプロジェクトを開始するときは、まず必要なディレクトリ構成を作成します。以下に例を示します。

```Fish Shell
# プロジェクトフォルダを作成
mkdir my_project

# プロジェクトフォルダに移動
cd my_project

# 必要なサブフォルダとファイルを作成
mkdir bin lib src tests
touch README.md LICENSE
```

実行後の出力はなく、ディレクトリとファイルが作成されていることが確認できます。

## Deep Dive (深い潜水)
Fish Shellは洗練されたスクリプト環境を提供するUnixシェルの一つです。歴史的にはBashやZshが人気でしたが、Fishはその独自の機能で注目を集めています。たとえば、自動補完や構文のハイライト機能があります。BashやZshスクリプトとは異なり、Fishでは関数と変数の扱いが簡単です。しかし、シェルスクリプトの移植性を重視する場合は、Bashがより一般的な選択かもしれません。

Fishでは、プロジェクトを開始するための内蔵コマンド「funced」と「funcsave」を利用して独自の関数を作成し、シェルセッション間で簡単にその関数を保持することができます。これにより、繰り返し利用するコマンドを簡単に定義できます。

## See Also (参照)
- Fishの公式ドキュメンテーション: https://fishshell.com/docs/current/index.html
- 機能比較のためのBash vs Fish: https://www.slant.co/versus/2208/5986/~bash_vs_fish
- FishのGitHubリポジトリ: https://github.com/fish-shell/fish-shell
- Fish Shellのスクリプト例とチュートリアル: https://fishshell.com/docs/current/tutorial.html
