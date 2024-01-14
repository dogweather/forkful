---
title:                "Arduino: 「yamlを扱う」"
simple_title:         "「yamlを扱う」"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/working-with-yaml.md"
---

{{< edit_this_page >}}

Arduinoプログラミングを始めるには?

## なぜ

あなたはArduinoプログラミングを学び始めることに興味がありますか？ そうしたら、YAMLの使用について知っておくことが重要です。YAMLは、テキストで構造化データを表現するための形式です。Arduinoプログラミングにおいても、様々なデータを保存するために使用することができます。

## ハウトゥー

まず、YAMLを使用するためにはライブラリが必要です。Arduinoプログラム内でYAMLライブラリを使用するには、まずライブラリをダウンロードしてインストールする必要があります。その後、```Arduino ... ```というコードブロック内で以下のようにライブラリをインクルードします。

```Arduino
#include <YAML.h>
```

YAMLライブラリを使用するための基本構文は以下の通りです。

```Arduino
YAML.begin(char* fileName); // YAMLファイルの読み込みを開始
YAML.load(doc); // YAMLファイルをロードして変数に格納
YAML.getvalue("key"); // 指定したキーに対応する値を取得
```

例えば、以下のようなYAMLファイルを作成した場合、

```
name: John
age: 25
city: Tokyo
```

上記の基本構文を使用して「age」の値を取得することができます。

```Arduino
int age = YAML.getvalue("age");
```

また、変数をYAMLファイルに書き込むことも可能です。

```Arduino
char* name = "Amy";
int age = 30;
char* city = "Osaka";

YAML.begin("myInfo.yaml");
YAML.add("name", name);
YAML.add("age", age);
YAML.add("city", city);
YAML.save("myInfo.yaml"); // 変更を保存
```

## ディープダイブ

YAMLファイルは、複数のデータを階層的に保存することができます。例えば、以下のようなデータを持つYAMLファイルを作成することができます。

```
person:
  name: John
  age: 25
  city: Tokyo

animal:
  name: Fluffy
  type: Dog
  age: 3
```

YAMLライブラリを使用する際には、階層構造に沿ってデータを取得することができます。

```Arduino
YAML.getvalue("person.name"); // Johnを取得
YAML.getvalue("animal.type"); // Dogを取得
```

また、YAMLファイル内に配列を作成することも可能です。

```
numbers:
  - 1
  - 2
  - 3
```

配列内のデータを取得する場合は、以下のように記述します。

```Arduino
int thirdNumber = YAML.getvalue("numbers[2]"); // 3を取得
```

さらに、YAMLファイル内にはコメントを追加することもできます。コメントは「#」で始まる行として記述します。

```
# このファイルには数値の配列が含まれています
numbers:
  # 以下は1から5までの数値が含まれています
  - 1
  - 2
  - 3
  - 4
  - 5
```

## シーアルソー

- YAMLライブラリのインストール方法: https://github.com/jandelgado/yaml-arduino/blob/master/doc/INSTALL.md
- YAML基本構文の詳細: https://yaml.org/spec/