---
date: 2024-01-20 18:04:43.584041-07:00
description: "\u65B0\u3057\u3044\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u3092\u59CB\u3081\
  \u308B\u3068\u306F\u3001\u30BC\u30ED\u304B\u3089\u30B3\u30FC\u30C9\u3092\u66F8\u304D\
  \u59CB\u3081\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\
  \u306F\u65B0\u3057\u3044\u30A2\u30A4\u30C7\u30A3\u30A2\u3092\u5B9F\u73FE\u3057\u305F\
  \u308A\u3001\u5B66\u3093\u3060\u308A\u3001\u554F\u984C\u3092\u89E3\u6C7A\u3059\u308B\
  \u305F\u3081\u306B\u65B0\u3057\u3044\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u3092\u59CB\
  \u3081\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.856054-06:00'
model: gpt-4-1106-preview
summary: "\u65B0\u3057\u3044\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u3092\u59CB\u3081\
  \u308B\u3068\u306F\u3001\u30BC\u30ED\u304B\u3089\u30B3\u30FC\u30C9\u3092\u66F8\u304D\
  \u59CB\u3081\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\
  \u306F\u65B0\u3057\u3044\u30A2\u30A4\u30C7\u30A3\u30A2\u3092\u5B9F\u73FE\u3057\u305F\
  \u308A\u3001\u5B66\u3093\u3060\u308A\u3001\u554F\u984C\u3092\u89E3\u6C7A\u3059\u308B\
  \u305F\u3081\u306B\u65B0\u3057\u3044\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u3092\u59CB\
  \u3081\u307E\u3059\u3002"
title: "\u65B0\u3057\u3044\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u3092\u59CB\u3081\u308B"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)

新しいプロジェクトを始めるとは、ゼロからコードを書き始めることです。プログラマーは新しいアイディアを実現したり、学んだり、問題を解決するために新しいプロジェクトを始めます。

## How to: (方法)

Rubyで新しいプロジェクトを始める時には、まずディレクトリ構造と基本ファイルを作るのが一般的です。以下のコマンドはシンプルなRubyプロジェクトを始める手順を示しています。

```Ruby
# Terminalで新しいディレクトリを作成
$ mkdir my_new_project

# プロジェクトのディレクトリに移動
$ cd my_new_project

# Gemfileを作成して、依存関係を管理
$ bundle init

# libディレクトリを作成し、コードを格納
$ mkdir lib

# libディレクトリ内でメインのRubyファイルを作成
$ touch lib/my_new_project.rb

# irbまたはpryで新しいファイルを試す
$ irb
> require './lib/my_new_project'
=> true
```

これで、新しいRubyプロジェクトの基礎ができました。

## Deep Dive (深掘り)

Rubyプロジェクトを始めるとき、ディレクトリ構造は後での開発をスムーズにするために重要です。`lib` ディレクトリはライブラリコードの標準的な場所です。`Gemfile`はBundlerを使って依存関係を管理するためのファイルです。

過去にはRubyのプロジェクトではRakefileや`.ruby-version` もよく使用されていました。バージョン管理はrbenvやRVMを利用して行われることが多いです。他にもRailsのようなフレームワークを使うことで、プロジェクトの雛形が提供されますが、小規模なスクリプトやライブラリでは上記のようにシンプルな構造から始めることが一般的です。

## See Also (関連情報)

- Rubyのオフィシャルサイト: [https://www.ruby-lang.org/ja/](https://www.ruby-lang.org/ja/)
- Bundlerのドキュメント: [https://bundler.io/](https://bundler.io/)
- RubyGemsの基本: [https://guides.rubygems.org/](https://guides.rubygems.org/)
- rbenvによるRubyバージョン管理: [https://github.com/rbenv/rbenv](https://github.com/rbenv/rbenv)
- RVM(Ruby Version Manager)の使い方: [https://rvm.io/](https://rvm.io/)
