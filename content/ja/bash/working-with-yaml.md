---
title:                "yamlとの作業方法"
html_title:           "Bash: yamlとの作業方法"
simple_title:         "yamlとの作業方法"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/working-with-yaml.md"
---

{{< edit_this_page >}}

## 何&なぜ？
YAML (YAML Ain't Markup Language) とは、構造化されたデータを人間が読みやすい形式で表現するためのフォーマットです。プログラマーがYAMLを使用する理由は、構造化されたデータを管理しやすくするためです。

## 方法：
以下に、BashプログラミングでYAMLを使用する方法の例を示します。

```
# YAMLファイルの読み込み
yaml_file = "sample.yaml"
cat $yaml_file

# YAMLファイルの書き込み
echo "person:
  name: John
  age: 30" > $yaml_file

# YAMLファイルの内容を変数に格納
yaml_content=$(cat $yaml_file)

# 変数からデータを取得
name=$(echo $yaml_content | grep "name" | cut -d ":" -f2)
echo "Name: $name"

# 数値を含むYAMLデータの扱い
array=(1, 2, 3, 4)
echo "array: $array" > $yaml_file
echo "Array with numbers: $(grep "array" $yaml_file | awk '{print $2}')"
```

上記コードの出力:

```
person:
  name: John
  age: 30
Name: John
Array with numbers: 1, 2, 3, 4
```

## 詳細：
YAMLは、ホームページやソフトウェアの設定ファイルなど、様々なアプリケーションで使用されてきました。他のフォーマットと比べて、YAMLはシンプルで読みやすい書き方ができることが特徴です。同様のフォーマットとしてJSONやXMLがありますが、YAMLはより人間にとって親しみやすく、データを管理しやすいと言われています。

## 参考：
[YAML.org](https://yaml.org/)

[YAMLの使い方](https://www.iana.org/protocols/yang/yang.html)

[YAMLの概要](https://ja.wikipedia.org/wiki/YAML)