---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:31.496858-07:00
description: "JSON\uFF08JavaScript Object Notation\uFF09\u3092\u6271\u3046\u3068\u3044\
  \u3046\u3053\u3068\u306F\u3001JSON\u5F62\u5F0F\u306E\u6587\u5B57\u5217\u3092Python\u30AA\
  \u30D6\u30B8\u30A7\u30AF\u30C8\u306B\u89E3\u6790\u3057\u305F\u308A\u3001\u305D\u306E\
  \u9006\u3092\u884C\u3063\u305F\u308A\u3059\u308B\u3053\u3068\u3092\u610F\u5473\u3057\
  \u307E\u3059\u3002\u3053\u308C\u306F\u3001JSON\u304C\u30B5\u30FC\u30D0\u30FC\u3068\
  \u30AF\u30E9\u30A4\u30A2\u30F3\u30C8\u9593\u306E\u30C7\u30FC\u30BF\u4EA4\u63DB\u306E\
  \u5171\u901A\u8A00\u8A9E\u3067\u3042\u308B\u305F\u3081\u3001Web\u3084API\u306E\u958B\
  \u767A\u306B\u4E0D\u53EF\u6B20\u3067\u3059\u3002"
lastmod: '2024-03-13T22:44:41.528265-06:00'
model: gpt-4-0125-preview
summary: "JSON\uFF08JavaScript Object Notation\uFF09\u3092\u6271\u3046\u3068\u3044\
  \u3046\u3053\u3068\u306F\u3001JSON\u5F62\u5F0F\u306E\u6587\u5B57\u5217\u3092Python\u30AA\
  \u30D6\u30B8\u30A7\u30AF\u30C8\u306B\u89E3\u6790\u3057\u305F\u308A\u3001\u305D\u306E\
  \u9006\u3092\u884C\u3063\u305F\u308A\u3059\u308B\u3053\u3068\u3092\u610F\u5473\u3057\
  \u307E\u3059\u3002\u3053\u308C\u306F\u3001JSON\u304C\u30B5\u30FC\u30D0\u30FC\u3068\
  \u30AF\u30E9\u30A4\u30A2\u30F3\u30C8\u9593\u306E\u30C7\u30FC\u30BF\u4EA4\u63DB\u306E\
  \u5171\u901A\u8A00\u8A9E\u3067\u3042\u308B\u305F\u3081\u3001Web\u3084API\u306E\u958B\
  \u767A\u306B\u4E0D\u53EF\u6B20\u3067\u3059\u3002."
title: "JSON\u3092\u6D3B\u7528\u3059\u308B"
weight: 38
---

## 何となぜ？

JSON（JavaScript Object Notation）を扱うということは、JSON形式の文字列をPythonオブジェクトに解析したり、その逆を行ったりすることを意味します。これは、JSONがサーバーとクライアント間のデータ交換の共通言語であるため、WebやAPIの開発に不可欠です。

## 方法:

Pythonの組み込み`json`ライブラリは、エンコーディング（PythonオブジェクトをJSONに変換）とデコーディング（JSONをPythonオブジェクトに変換）の過程を簡素化します。以下の方法で使用できます：

### PythonオブジェクトをJSONにエンコーディング：

```python
import json

data = {
    "name": "John Doe",
    "age": 30,
    "isEmployee": True,
    "addresses": [
        {"city": "New York", "zipCode": "10001"},
        {"city": "San Francisco", "zipCode": "94016"}
    ]
}

json_string = json.dumps(data, indent=4)
print(json_string)
```

**出力:**

```json
{
    "name": "John Doe",
    "age": 30,
    "isEmployee": true,
    "addresses": [
        {
            "city": "New York",
            "zipCode": "10001"
        },
        {
            "city": "San Francisco",
            "zipCode": "94016"
        }
    ]
}
```

### JSONをPythonオブジェクトにデコーディング：

```python
json_string = '''
{
    "name": "John Doe",
    "age": 30,
    "isEmployee": true,
    "addresses": [
        {
            "city": "New York",
            "zipCode": "10001"
        },
        {
            "city": "San Francisco",
            "zipCode": "94016"
        }
    ]
}
'''

data = json.loads(json_string)
print(data)
```

**出力:**

```python
{
    'name': 'John Doe', 
    'age': 30, 
    'isEmployee': True, 
    'addresses': [
        {'city': 'New York', 'zipCode': '10001'}, 
        {'city': 'San Francisco', 'zipCode': '94016'}
    ]
}
```

### サードパーティのライブラリの使用:

スキーマ検証やURLから直接JSONファイルを解析するなど、より複雑なJSON処理には、HTTPリクエスト用の`requests`や検証用の`jsonschema`などのライブラリが役立ちます。

#### `requests`を使ってURLからJSONを解析する例：

```python
import requests

response = requests.get('https://api.example.com/data')
data = response.json()

print(data)
```

このスニペットは、指定されたURLからJSONデータを取得し、直接Pythonオブジェクトに変換します。

#### `jsonschema`を使用してJSONを検証する：

まず、pipを介してライブラリをインストールします：

```bash
pip install jsonschema
```

次に、以下のように使用します：

```python
from jsonschema import validate
import jsonschema

schema = {
    "type": "object",
    "properties": {
        "name": {"type": "string"},
        "age": {"type": "number"},
        "isEmployee": {"type": "boolean"},
    },
    "required": ["name", "age", "isEmployee"]
}

# `data`がJSONデコードから得られた辞書であると仮定します
try:
    validate(instance=data, schema=schema)
    print("Valid JSON data.")
except jsonschema.exceptions.ValidationError as err:
    print("Validation error:", err)
```

この例では、デコードされたJSONデータから得られたPython辞書を、あらかじめ定義されたスキーマに対して検証し、データが期待されるフォーマットやタイプに準拠していることを確認します。
