---
title:                "Ruby: 新しいプロジェクトを開始する"
programming_language: "Ruby"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/starting-a-new-project.md"
---

{{< edit_this_page >}}

【## なぜ】

新しいプロジェクトを始める理由には様々なものがあります。 例えば、新しいプログラミング言語を学んだり、興味のある問題を解決するために自分でアプリケーションを作ることができます。どのような理由であれ、プロジェクトを始めることで新しいスキルを習得することができるので、挑戦する価値があります。

【## 作り方】

新しいプロジェクトを始めるとき、まずはプロジェクトの目的を明確にしましょう。次に、必要なツールやライブラリをインストールし、プロジェクトのフォルダーを作成します。その後、コーディングを始める前に、プロジェクトの構成を考えることが重要です。最初の試みだからといって、コードが複雑になりすぎるのは避けたいところです。プロジェクトが変化する可能性があることも念頭に置いておくと良いでしょう。

以下は、Rubyでプロジェクトを始める際の一例です。まずは「Gemfile」というファイルを作成し、使用するライブラリを指定します。

```Ruby
source 'https://rubygems.org'
gem ‘sinatra’
gem ‘nokogiri’
gem ‘pg’
```

作成したGemfileを保存した後、必要なライブラリをインストールします。

```
$ bundle install
```

次に、プロジェクトのフォルダー内に「app.rb」というファイルを作成し、コーディングを始めましょう。

```Ruby
require ‘sinatra’
require ‘nokogiri’
require ‘pg’

get ‘/’ do
  “Hello world!”
end
```

以上のコードを実行すると、`Hello world!`というメッセージが表示されるはずです。この例では、sinatraやnokogiri、pgというライブラリを使用していますが、あなたが使いたいライブラリを自由に選択することができます。

【## 深堀り】

新しいプロジェクトを始めるとき、最初の試みだからといって完璧なコードを書くことはできません。しかし、それは正常なことです！初めてのプロジェクトの場合、コードを書くこと自体が学びになるので、自分のコードに自信を持って進めましょう。

また、Rubyのコミュニティは大きく、多くの有益なリソースがあります。例えば、Ruby公式ドキュメントやオンライン上のRubyのチュートリアルなどがあります。また、Stack OverflowやGitHubなどのコミュニティサイトでも質問やコードの共有ができるので、自分のプロジェクトについてどのような形で学ぶことができるか考えてみましょう。

【# もっと見る】

- [Ruby 公式ドキュメント](https://www.ruby-lang.org/ja/documentation/)
- [Rubyチュートリアル](https://www.tutorialspoint.com/ruby/)
- [Stack Overflow (Ruby)](https://stackoverflow.com/questions/tagged/ruby)
- [GitHub](https://github.com/)