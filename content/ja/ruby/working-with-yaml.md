---
title:                "「YAMLを使ったプログラミング」"
html_title:           "Ruby: 「YAMLを使ったプログラミング」"
simple_title:         "「YAMLを使ったプログラミング」"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/working-with-yaml.md"
---

{{< edit_this_page >}}

## なぜYAMLを使うのか
YAMLは、データを人間が読みやすい形式で記述することができるため、プログラム開発において広く使われています。また、コード内での記述もシンプルであり、柔軟かつ拡張性の高いフォーマットであるため、開発者の間で人気があります。

## 使い方
YAMLをRubyで扱う際には、"yaml"ライブラリを使用する必要があります。以下のコードを参考にしてください。

```Ruby
require 'yaml'

# YAMLデータの読み込み
data = YAML.load_file('data.yaml')

# データの書き出し
puts data.to_yaml

# データの編集
data['name'] = 'John'

# 編集したデータをファイルに書き出し
File.open('data.yaml', 'w') { |f| f.write(data.to_yaml) }

# データの削除
data.delete('age')

# 削除したデータをコンソールに出力
puts data.to_yaml
```

実際の出力は以下のようになります。

```YAML
name: Jane
age: 25
hobby: reading

---
name: John
age: 25
hobby: reading

---
name: John
hobby: reading
```

## ディープダイブ
YAMLには、様々なデータ型を表現する方法があります。基本的なデータ型の他に、配列やハッシュ、さらにはクラスのオブジェクトまで表現することができます。また、YAMLの主な使用目的は設定ファイルやデータ保存ではなく、データの転送や共有にあります。そのため、プログラミング言語やプラットフォームの差異を気にせずにデータを受け渡すことができる利点があります。

## 参考になるリンク
- [RubyでYAMLを扱う方法](https://docs.ruby-lang.org/ja/2.6.0/library/yaml.html)
- [YAMLを使ったデータのシリアライズとデシリアライズ](https://reffect.co.jp/ruby/ruby-yaml-serialize-deserialize-data)
- [Why You Should Use YAML for Configuration](https://dev.to/mandrewcito/why-you-should-use-yaml-for-configuration-2632)