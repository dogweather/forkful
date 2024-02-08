---
title:                "TOMLを扱う方法"
aliases:
- ja/ruby/working-with-toml.md
date:                  2024-01-26T04:26:18.072182-07:00
model:                 gpt-4-0125-preview
simple_title:         "TOMLを扱う方法"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/working-with-toml.md"
---

{{< edit_this_page >}}

## 何となぜ?

TOMLは、その明確な意味論により読みやすい設定ファイル形式です。プログラマーは、XMLの重さやYAMLの気まぐれさなしに、アプリの設定やデータのシリアライゼーションを管理するためにTOMLを使用します。

## どのように:

まず、`toml-rb`ジェムをインストールします。これはRubyでTOMLを解析するための人気選択です。

```Ruby
gem install toml-rb
```

次に、TOMLファイルを読む：

```Ruby
require 'toml-rb'

toml_content = File.read('config.toml')
config = TomlRB.parse(toml_content)
puts config['title']
```

サンプル出力は次のようになるかもしれません：

```
My Awesome App
```

TOMLファイルに書き込む：

```Ruby
require 'toml-rb'

config = {
  'title' => 'My Awesome App',
  'owner' => {
    'name' => 'John Doe',
    'dob' => Date.new(1979, 5, 27)
  }
}

toml_string = TomlRB.dump(config)
File.write('config.toml', toml_string)
```

`config.toml`をチェックすると、設定がきれいに保存されているのがわかります。

## ディープダイブ

TOMLとは、Tom's Obvious, Minimal Languageの略で、GitHubの共同創設者であるTom Preston-Wernerによって2013年頃に作られました。その主な目的は、データ構造に簡単に解析できる直接的な形式であることです。JSONはAPIには素晴らしく、YAMLは柔軟ですが、TOMLのニッチは人間に優しいことに重点を置いています。YAMLがインデントにうるさいのに対し、TOMLは多くの人がよりシンプルでエラーが少ないと感じるINIライクな構造を目指しています。

JSON、YAML、XMLなどの代替手段はそれぞれに強みがありますが、TOMLは設定を人間とプログラムの両方によって簡単に維持できるシナリオで活躍します。これはより単純なだけでなく、厳格で読みやすいフォーマットを強制します。

技術的な側面では、RubyでTOMLコンテンツを解析するために、私たちは`toml-rb`のようなジェムを活用しています。このジェムはRubyの動的な性質を活かし、TOMLデータをネイティブのRubyのハッシュ、配列、その他の基本的なデータ構造に変換します。この変換は、開発者がTOMLデータを馴染みのあるRubyの意味論や方法で操作できることを意味します。

## 参照

- TOMLプロジェクトと仕様: https://toml.io/en/
- `toml-rb`ジェム: https://github.com/emancu/toml-rb
- TOML、YAML、JSONを比較: https://blog.theodo.com/2021/08/compare-yml-toml-json/
