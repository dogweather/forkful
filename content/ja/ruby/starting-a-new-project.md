---
title:                "新しいプロジェクトを始める"
date:                  2024-01-20T18:04:43.584041-07:00
model:                 gpt-4-1106-preview
simple_title:         "新しいプロジェクトを始める"

category:             "Ruby"
tag:                  "Getting Started"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/starting-a-new-project.md"
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
