---
date: 2024-01-26 04:26:18.072182-07:00
description: "\u3069\u306E\u3088\u3046\u306B: \u307E\u305A\u3001`toml-rb`\u30B8\u30A7\
  \u30E0\u3092\u30A4\u30F3\u30B9\u30C8\u30FC\u30EB\u3057\u307E\u3059\u3002\u3053\u308C\
  \u306FRuby\u3067TOML\u3092\u89E3\u6790\u3059\u308B\u305F\u3081\u306E\u4EBA\u6C17\
  \u9078\u629E\u3067\u3059\u3002"
lastmod: '2024-03-13T22:44:42.886961-06:00'
model: gpt-4-0125-preview
summary: "\u307E\u305A\u3001`toml-rb`\u30B8\u30A7\u30E0\u3092\u30A4\u30F3\u30B9\u30C8\
  \u30FC\u30EB\u3057\u307E\u3059\u3002\u3053\u308C\u306FRuby\u3067TOML\u3092\u89E3\
  \u6790\u3059\u308B\u305F\u3081\u306E\u4EBA\u6C17\u9078\u629E\u3067\u3059."
title: "TOML\u3092\u6271\u3046\u65B9\u6CD5"
weight: 39
---

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
