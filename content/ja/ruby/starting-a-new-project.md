---
title:                "新しいプロジェクトを始める"
html_title:           "Ruby: 新しいプロジェクトを始める"
simple_title:         "新しいプロジェクトを始める"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why?
プロジェクトをスタートさせるとは、新しいアプリケーションやプログラムを開始することを意味します。プログラマーが新しいプロジェクトを始める理由は、新しいアイデアや技術を試したり、既存のコードを改善したりするためです。

## How to:
### 新しいプロジェクトの作成
新しいプロジェクトを作成するためには、まず新しいディレクトリを作成します。次に、ターミナルでそのディレクトリに移動し、以下のコマンドを実行します。
```
$ mkdir new_project
$ cd new_project
```

### プロジェクトの初期化
新しいプロジェクトを初期化するためには、プロジェクト内で以下のコマンドを実行します。
```
$ bundle init
```
このコマンドにより、Gemfileが作成されます。

### 必要なGemをインストール
プロジェクトに必要なGemをインストールするには、Gemfileに以下のように追加します。
```
gem 'gem_name'
```
そして、以下のコマンドを実行します。
```
$ bundle install
```
これにより、必要なGemがインストールされます。

## Deep Dive:
Starting a new project has been made more efficient with the use of tools such as the Bundler gem, which helps manage dependencies and allows for a more streamlined process. Other alternatives to starting a new project include cloning an existing project and building on top of it or using a project generator tool like Rails.

## See Also:
- [Bundler](https://bundler.io/)
- [Rails](https://rubyonrails.org/)
- [Git Clone](https://git-scm.com/docs/git-clone)