---
title:    "Ruby: 新しいプロジェクトを始める"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/starting-a-new-project.md"
---

{{< edit_this_page >}}

### なぜ
新しいプロジェクトを始めることに関わるメリットを知るために、この記事をご覧ください。

### 方法
新しいプロジェクトを始める方法をいくつかのコーディング例とともにご紹介します。

```Ruby
# プロジェクトの作成
project = Project.new("ruby_projet")

# プロジェクトにファイルを追加
project.add_file("main.rb")

# プロジェクトのファイルを表示
puts project.files

# 出力結果
["main.rb"]
```

### 深くダイブ
新しいプロジェクトを始めるときのさらに詳しい情報をご紹介します。プロジェクトの目的やターゲットユーザーを明確にすることで、プロジェクトの計画を立てる上での有益な情報となるでしょう。

また、プロジェクトの名前やファイル構成などもしっかりと決めることで、プロジェクトをよりスムーズに進捗させることができるでしょう。

### 参考リンク
[プログラミング言語Ruby](https://www.ruby-lang.org/ja/)
[Rubyのインストール方法](https://www.sejuku.net/blog/9375)
[Rubyで作られた有名なプロジェクト](https://qiita.com/shibukawa/items/78e9581cb054555a8546)