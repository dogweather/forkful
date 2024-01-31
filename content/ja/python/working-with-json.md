---
title:                "JSONを扱う方法"
date:                  2024-01-19
html_title:           "Arduino: JSONを扱う方法"
simple_title:         "JSONを扱う方法"

category:             "Python"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
### なぜJSONを扱うのか?
JSONはデータ交換のフォーマットです。読みやすく書きやすいため、APIや設定ファイルで広く使われています。

## How to:
### やり方:
```Python
import json

# JSON文字列をPythonの辞書に変換する
json_string = '{"name": "Tanaka", "age": 30, "is_student": false}'
data = json.loads(json_string)
print(data)

# Pythonの辞書をJSON文字列に変換する
python_dict = {'name': 'Sato', 'age': 25, 'is_student': True}
json_data = json.dumps(python_dict, ensure_ascii=False, indent=2)
print(json_data)
```

出力:
```
{'name': 'Tanaka', 'age': 30, 'is_student': False}
{
  "name": "Sato",
  "age": 25,
  "is_student": true
}
```

## Deep Dive
### 深掘り:
JSONはJavaScript Object Notationの略で、元々はJavaScriptのオブジェクト記法に由来します。しかし、そのシンプルさから非JavaScript環境でも採用されています。XMLはJSONの代替として使われることもありますが、JSONのほうが扱いやすく軽量です。Pythonでは`json`モジュールを用いて簡単にJSONデータを扱うことが可能になっており、`load`と`loads`で読み込み、`dump`と`dumps`で出力できます。

## See Also
### 参照:
- 公式ドキュメント: https://docs.python.org/3/library/json.html
- JSONの仕様: https://www.json.org/json-ja.html
- W3SchoolsのJSONチュートリアル: https://www.w3schools.com/js/js_json_intro.asp
