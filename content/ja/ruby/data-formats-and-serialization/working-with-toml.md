---
aliases:
- /ja/ruby/working-with-toml/
date: 2024-01-26 04:26:18.072182-07:00
description: "TOML\u306F\u3001\u305D\u306E\u660E\u78BA\u306A\u610F\u5473\u8AD6\u306B\
  \u3088\u308A\u8AAD\u307F\u3084\u3059\u3044\u8A2D\u5B9A\u30D5\u30A1\u30A4\u30EB\u5F62\
  \u5F0F\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001XML\u306E\
  \u91CD\u3055\u3084YAML\u306E\u6C17\u307E\u3050\u308C\u3055\u306A\u3057\u306B\u3001\
  \u30A2\u30D7\u30EA\u306E\u8A2D\u5B9A\u3084\u30C7\u30FC\u30BF\u306E\u30B7\u30EA\u30A2\
  \u30E9\u30A4\u30BC\u30FC\u30B7\u30E7\u30F3\u3092\u7BA1\u7406\u3059\u308B\u305F\u3081\
  \u306BTOML\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002"
lastmod: 2024-02-18 23:08:55.412663
model: gpt-4-0125-preview
summary: "TOML\u306F\u3001\u305D\u306E\u660E\u78BA\u306A\u610F\u5473\u8AD6\u306B\u3088\
  \u308A\u8AAD\u307F\u3084\u3059\u3044\u8A2D\u5B9A\u30D5\u30A1\u30A4\u30EB\u5F62\u5F0F\
  \u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001XML\u306E\u91CD\
  \u3055\u3084YAML\u306E\u6C17\u307E\u3050\u308C\u3055\u306A\u3057\u306B\u3001\u30A2\
  \u30D7\u30EA\u306E\u8A2D\u5B9A\u3084\u30C7\u30FC\u30BF\u306E\u30B7\u30EA\u30A2\u30E9\
  \u30A4\u30BC\u30FC\u30B7\u30E7\u30F3\u3092\u7BA1\u7406\u3059\u308B\u305F\u3081\u306B\
  TOML\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002"
title: "TOML\u3092\u6271\u3046\u65B9\u6CD5"
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
