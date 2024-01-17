---
title:                "jsonを利用する"
html_title:           "Python: jsonを利用する"
simple_title:         "jsonを利用する"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/working-with-json.md"
---

{{< edit_this_page >}}

## 何と何故？

JSONとは、プログラマーがデータを保存、転送、または共有するために使用するフォーマットの一種です。JSONは、さまざまな言語やプログラムによって簡単に読み取ることができるので、広く使用されています。

## 方法：

Pythonを使用してJSONを扱う方法を見てみましょう。
```
# JSONモジュールをインポートする
import json

# JSONデータを文字列として定義する
json_data = '{"name": "John", "age": 30, "city": "Tokyo"}'

# JSONをPythonの辞書に変換する
python_data = json.loads(json_data)

# 辞書のキーと値を出力する
print(python_data["name"])
print(python_data["age"])
print(python_data["city"])

```
出力：
```
John
30
Tokyo
```

## 深層スクラップ

JSONは、JavaScript Object Notationの頭字語であり、1999年に開発されました。それ以来、JSONはWeb開発やデータの共有に広く使用されてきました。代わりに、XMLを使用することもできますが、JSONの方がよりシンプルであり、データの整形と読み取りも容易です。Python以外にも、JavaScriptやJava、PHPなど、さまざまなプログラム言語でJSONを使用することができます。

## 関連リンク

- JSON公式サイト: https://www.json.org/json-en.html
- Python公式ドキュメント: https://docs.python.org/3/library/json.html