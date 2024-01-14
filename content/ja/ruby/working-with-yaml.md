---
title:                "Ruby: 「yamlを使うプログラミング」"
simple_title:         "「yamlを使うプログラミング」"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/working-with-yaml.md"
---

{{< edit_this_page >}}

# なぜYAMLを使用するのか

YAMLは、データの構造を記述するための人間にとって読みやすいフォーマットです。これは、プログラマーがデータ構造を作成するのに役立ちます。Rubyプログラミングで使用すると、構造化されたデータをより簡単に管理できるようになります。

## 使い方

YAMLを使用する最も簡単な方法は、YAMLライブラリをインストールして、loadメソッドでYAMLファイルを読み込むことです。以下のコード例を参考にしてください。

```
# YAMLライブラリを読み込む
require 'yaml'

# YAMLファイルを読み込む
data = YAML.load(File.read('data.yml'))

# データを出力する
puts data
```

YAMLファイルの内容：

```
---
people:
  - name: 山田太郎
    age: 30
    occupation: プログラマー
  - name: 鈴木次郎
    age: 25
    occupation: デザイナー
```

出力結果：

```
{"people"=>[{"name"=>"山田太郎", "age"=>30, "occupation"=>"プログラマー"}, {"name"=>"鈴木次郎", "age"=>25, "occupation"=>"デザイナー"}]}
```

## ディープダイブ

YAMLファイルは、ネストされたデータ構造を持つことができます。これは、データの階層構造を表現するのに非常に便利です。また、YAMLファイルはコメントを含むことができ、ファイルをより読みやすくすることができます。

さらに、Rubyでは、YAMLファイルから直接オブジェクトを作成することもできます。以下のコード例を参考にしてください。

```
# YAMLライブラリを読み込む
require 'yaml'

# YAMLファイルからオブジェクトを作成する
class Person
  def initialize(name, age, occupation)
    @name = name
    @age = age
    @occupation = occupation
  end
end

# YAMLファイルを読み込む
data = YAML.load(File.read('data.yml'))

# オブジェクトを作成する
people = data["people"].map do |person|
  Person.new(person["name"], person["age"], person["occupation"])
end

# データを出力する
puts people
```

出力結果：

```
[#<Person:0x00007fb1a1060118 @name="山田太郎", @age=30, @occupation="プログラマー">, #<Person:0x00007fb1a105fd20 @name="鈴木次郎", @age=25, @occupation="デザイナー">]
```

## 関連リンク

- [YAMLの公式ドキュメント（英語）](https://yaml.org/spec/)
- [YAMLを使った設定ファイルの読み込み方（日本語）](https://qiita.com/tana2034/items/0b0b6fd11d76934f31c2)
- [RubyでYAMLを扱う方法（日本語）](https://qiita.com/ohtomi/items/07573c7fd5b083119077)