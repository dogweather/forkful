---
title:                "JSONを活用する"
aliases:
- /ja/python/working-with-json/
date:                  2024-02-03T19:24:31.496858-07:00
model:                 gpt-4-0125-preview
simple_title:         "JSONを活用する"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
