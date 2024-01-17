---
title:                "「yamlを使う」"
html_title:           "Ruby: 「yamlを使う」"
simple_title:         "「yamlを使う」"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/working-with-yaml.md"
---

{{< edit_this_page >}}

# なに&なぜ?

YAMLとは、テキスト形式のデータを表現するためのフォーマットです。プログラマーたちは、YAMLを使うことでデータを簡単に読み書きできるようになり、コードをより見やすく、管理しやすくすることができます。

## 使い方:

YAMLを使う最も簡単な方法は、YAMLの構文を覚えて、直接書き込むことです。しかし、RubyにはYAMLの構文を扱うための便利なライブラリがあります。

```Ruby
require 'yaml'              # YAMLライブラリを読み込む
obj = {foo: 'bar', baz: 1}  # ハッシュオブジェクトを作成
yaml_str = YAML.dump(obj)   # ハッシュをYAMLに変換
puts yaml_str               # YAMLを表示
```

出力:

```YAML
---
:foo: bar
:baz: 1
```

## より詳しく見てみる:

YAMLは、もともと2001年に公開された、オープンソースのデータフォーマットです。JSONやXMLと同様に、データの表現に特化したフォーマットであり、読みやすく、記述が簡単です。データのシリアライズや設定ファイルの保存によく使われています。

YAML以外にもデータ表現のフォーマットはありますが、YAMLの特徴はインデントによる構造の表現が可能であることです。これにより、データの階層関係を直感的に表現できます。また、ハッシュや配列などの任意のデータ型をサポートしているため、柔軟にデータを扱うことができます。

YAMLはRubyで実装されており、Ruby以外にもPythonやJavaScriptなど、多くのプログラミング言語で使用することができます。

## 関連リンク:

- [公式YAMLサイト](https://yaml.org)
- [YAMLライブラリドキュメント](https://ruby-doc.org/stdlib-2.7.2/libdoc/yaml/rdoc/YAML.html)