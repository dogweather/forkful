---
title:                "新しいプロジェクトを始める"
html_title:           "C: 新しいプロジェクトを始める"
simple_title:         "新しいプロジェクトを始める"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 何となぜ？

新規プロジェクトの開始とは、新たなアイデアを形にする手続きのことを指します。なぜプログラマーはこれを行うのでしょうか？それは、新たな解決策を創造し、生産性を高め、技術的なスキルを向上させるためです。

## 手順：

新しいRubyプロジェクトを開始するための基本的なステップを以下に示します。

```Ruby
# ディレクトリ作成
$ mkdir new_project
$ cd new_project

# Bundlerのインストール
$ gem install bundler

# 新しいGemfileの作成
$ bundle init
```
これらのコマンドを実行すると、`new_project`という名前の新しいディレクトリが作成され、その中に新しい`Gemfile`が作成されます。

## 深層探討：

新規プロジェクトの開始は、1970年代から現在までのプログラミングの歴史と共に発展してきました。当時も今日も、プログラマは新しいアイデアを探求し、技術的な成果を共有することを楽しみにしています。しかし、Rubyや他の現代の言語が提供するような高度なツールは存在しなかったので、新しいプロジェクトの開始は困難なプロセスでした。

代替手段としては、他の言語（Python、Javaなど）やフレームワーク（Rails、Sinatraなど）を使用することがあります。各手段にはそれぞれ利点と欠点があります。

具体的な実装については、`gem install bundler`はRubyのgemを管理するツールで、`bundle init`は新しいGemfileを作成します。このファイルは、プロジェクトの依存関係を管理するために使用されます。

## 参考リンク：

新しいプロジェクトの開始についてのさらなる情報は、以下のリンクから得ることができます。

- Ruby公式ドキュメンテーション: https://www.ruby-lang.org/ja/documentation/
- Bundler公式ドキュメンテーション: https://bundler.io/
- プロジェクト開始についてのステップバイステップガイド: https://guides.rubyonrails.org/getting_started.html