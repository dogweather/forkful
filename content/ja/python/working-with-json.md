---
title:                "「JSONの操作」"
html_title:           "Python: 「JSONの操作」"
simple_title:         "「JSONの操作」"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/working-with-json.md"
---

{{< edit_this_page >}}

## なぜJSONを使うのか

Pythonは非常に人気の高いプログラミング言語であり、データの扱いにおいても非常に優れています。その中でも、JSONはデータの形式として広く使われており、Pythonでも簡単に扱うことができます。JSONを扱えるようになることで、より多くのデータを取り扱えるようになり、より高度なプログラミングが可能になります。

## JSONの使い方

JSONを扱うためには、Pythonに組み込まれているjsonモジュールを使用します。まずは、データをjson形式に変換する方法から見ていきましょう。

```Python
import json

# データの定義
data = {
  "name": "John",
  "age": 30,
  "hobbies": ["reading", "sports", "cooking"]
}

# json形式に変換
json_data = json.dumps(data)

# 出力
print(json_data)
```

出力結果は以下の通りになります。

```Python
'{"name": "John", "age": 30, "hobbies" : ["reading", "sports", "cooking"]}'
```

次に、json形式からPythonのデータ型に変換する方法を見ていきましょう。

```Python
import json

# JSONデータの定義
json_data = '{"name": "Lisa", "age": 25, "hobbies": ["music", "travel", "dancing"]}'

# Pythonのデータ型に変換
data = json.loads(json_data)

# 出力
print(data)
```

出力結果は以下の通りになります。

```Python
{'name': 'Lisa', 'age': 25, 'hobbies': ['music', 'travel', 'dancing']}
```

## JSONの詳細

JSONはテキスト形式でデータを表現するため、データを扱いやすく、簡単に解析することができます。また、Pythonではdict（辞書）やlist（リスト）といったデータ型を使用することで、JSONを簡単に扱うことができます。

さらに、Pythonではjsonモジュールを使用することで、ファイルから直接データを読み込んだり、ファイルにデータを書き込んだりすることもできます。

## 関連リンク

- [Python 公式ドキュメント - JSON](https://docs.python.org/ja/3/library/json.html)
- [JSONとは？基本的な使い方や実際の例を解説！](https://www.sejuku.net/blog/54492)
- [PythonでJSONデータを扱う方法](https://www.codexa.net/how-to-use-json-file-in-python/)