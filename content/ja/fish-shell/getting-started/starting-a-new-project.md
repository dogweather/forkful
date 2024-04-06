---
date: 2024-01-20 18:03:46.636467-07:00
description: "How to: (\u65B9\u6CD5) Fish Shell\u3067\u65B0\u3057\u3044\u30D7\u30ED\
  \u30B8\u30A7\u30AF\u30C8\u3092\u958B\u59CB\u3059\u308B\u3068\u304D\u306F\u3001\u307E\
  \u305A\u5FC5\u8981\u306A\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u69CB\u6210\u3092\u4F5C\
  \u6210\u3057\u307E\u3059\u3002\u4EE5\u4E0B\u306B\u4F8B\u3092\u793A\u3057\u307E\u3059\
  \u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.520255-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) Fish Shell\u3067\u65B0\u3057\u3044\u30D7\u30ED\u30B8\u30A7\
  \u30AF\u30C8\u3092\u958B\u59CB\u3059\u308B\u3068\u304D\u306F\u3001\u307E\u305A\u5FC5\
  \u8981\u306A\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u69CB\u6210\u3092\u4F5C\u6210\u3057\
  \u307E\u3059\u3002\u4EE5\u4E0B\u306B\u4F8B\u3092\u793A\u3057\u307E\u3059\u3002"
title: "\u65B0\u3057\u3044\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u3092\u59CB\u3081\u308B"
weight: 1
---

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
