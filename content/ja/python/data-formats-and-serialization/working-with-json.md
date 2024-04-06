---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:31.496858-07:00
description: "\u65B9\u6CD5: Python\u306E\u7D44\u307F\u8FBC\u307F`json`\u30E9\u30A4\
  \u30D6\u30E9\u30EA\u306F\u3001\u30A8\u30F3\u30B3\u30FC\u30C7\u30A3\u30F3\u30B0\uFF08\
  Python\u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\u3092JSON\u306B\u5909\u63DB\uFF09\u3068\
  \u30C7\u30B3\u30FC\u30C7\u30A3\u30F3\u30B0\uFF08JSON\u3092Python\u30AA\u30D6\u30B8\
  \u30A7\u30AF\u30C8\u306B\u5909\u63DB\uFF09\u306E\u904E\u7A0B\u3092\u7C21\u7D20\u5316\
  \u3057\u307E\u3059\u3002\u4EE5\u4E0B\u306E\u65B9\u6CD5\u3067\u4F7F\u7528\u3067\u304D\
  \u307E\u3059\uFF1A."
lastmod: '2024-03-13T22:44:41.528265-06:00'
model: gpt-4-0125-preview
summary: "Python\u306E\u7D44\u307F\u8FBC\u307F`json`\u30E9\u30A4\u30D6\u30E9\u30EA\
  \u306F\u3001\u30A8\u30F3\u30B3\u30FC\u30C7\u30A3\u30F3\u30B0\uFF08Python\u30AA\u30D6\
  \u30B8\u30A7\u30AF\u30C8\u3092JSON\u306B\u5909\u63DB\uFF09\u3068\u30C7\u30B3\u30FC\
  \u30C7\u30A3\u30F3\u30B0\uFF08JSON\u3092Python\u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\
  \u306B\u5909\u63DB\uFF09\u306E\u904E\u7A0B\u3092\u7C21\u7D20\u5316\u3057\u307E\u3059\
  \u3002\u4EE5\u4E0B\u306E\u65B9\u6CD5\u3067\u4F7F\u7528\u3067\u304D\u307E\u3059\uFF1A\
  \n"
title: "JSON\u3092\u6D3B\u7528\u3059\u308B"
weight: 38
---

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
