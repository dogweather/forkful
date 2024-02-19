---
aliases:
- /ja/fish-shell/starting-a-new-project/
date: 2024-01-20 18:03:46.636467-07:00
description: "\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u306E\u958B\u59CB\u3068\u306F\u3001\
  \u65B0\u3057\u3044\u30A2\u30A4\u30C7\u30A2\u3084\u30BD\u30EA\u30E5\u30FC\u30B7\u30E7\
  \u30F3\u306B\u57FA\u3065\u3044\u3066\u30B3\u30FC\u30C9\u3092\u66F8\u304D\u59CB\u3081\
  \u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u65B0\
  \u3057\u3044\u6A5F\u80FD\u3092\u63D0\u4F9B\u3001\u554F\u984C\u3092\u89E3\u6C7A\u3001\
  \u307E\u305F\u306F\u5358\u306B\u5B66\u7FD2\u3068\u63A2\u7A76\u3092\u9032\u3081\u308B\
  \u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:55.310158
model: gpt-4-1106-preview
summary: "\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u306E\u958B\u59CB\u3068\u306F\u3001\
  \u65B0\u3057\u3044\u30A2\u30A4\u30C7\u30A2\u3084\u30BD\u30EA\u30E5\u30FC\u30B7\u30E7\
  \u30F3\u306B\u57FA\u3065\u3044\u3066\u30B3\u30FC\u30C9\u3092\u66F8\u304D\u59CB\u3081\
  \u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u65B0\
  \u3057\u3044\u6A5F\u80FD\u3092\u63D0\u4F9B\u3001\u554F\u984C\u3092\u89E3\u6C7A\u3001\
  \u307E\u305F\u306F\u5358\u306B\u5B66\u7FD2\u3068\u63A2\u7A76\u3092\u9032\u3081\u308B\
  \u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
title: "\u65B0\u3057\u3044\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u3092\u59CB\u3081\u308B"
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
