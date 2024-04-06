---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:33.277206-07:00
description: "\u3069\u306E\u3088\u3046\u306B\u3057\u3066: Python\u3067YAML\u3092\u8AAD\
  \u307F\u66F8\u304D\u3059\u308B\u306B\u306F\u901A\u5E38\u3001\u30B5\u30FC\u30C9\u30D1\
  \u30FC\u30C6\u30A3\u306E\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u4F7F\u7528\u3057\u307E\
  \u3059\u3002\u305D\u306E\u4E2D\u3067`PyYAML`\u304C\u6700\u3082\u4EBA\u6C17\u304C\
  \u3042\u308A\u307E\u3059\u3002\u306F\u3058\u3081\u306B\u3001`pip install PyYAML`\u3092\
  \u5B9F\u884C\u3057\u3066PyYAML\u3092\u30A4\u30F3\u30B9\u30C8\u30FC\u30EB\u3059\u308B\
  \u5FC5\u8981\u304C\u3042\u308A\u307E\u3059\u3002 **\u4F8B: YAML\u30D5\u30A1\u30A4\
  \u30EB\u3078\u306E\u66F8\u304D\u8FBC\u307F**."
lastmod: '2024-04-05T22:37:49.861034-06:00'
model: gpt-4-0125-preview
summary: "Python\u3067YAML\u3092\u8AAD\u307F\u66F8\u304D\u3059\u308B\u306B\u306F\u901A\
  \u5E38\u3001\u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\u306E\u30E9\u30A4\u30D6\u30E9\
  \u30EA\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002\u305D\u306E\u4E2D\u3067`PyYAML`\u304C\
  \u6700\u3082\u4EBA\u6C17\u304C\u3042\u308A\u307E\u3059\u3002\u306F\u3058\u3081\u306B\
  \u3001`pip install PyYAML`\u3092\u5B9F\u884C\u3057\u3066PyYAML\u3092\u30A4\u30F3\
  \u30B9\u30C8\u30FC\u30EB\u3059\u308B\u5FC5\u8981\u304C\u3042\u308A\u307E\u3059\u3002"
title: "YAML \u3092\u64CD\u4F5C\u3059\u308B"
weight: 41
---

## どのようにして:
PythonでYAMLを読み書きするには通常、サードパーティのライブラリを使用します。その中で`PyYAML`が最も人気があります。はじめに、`pip install PyYAML`を実行してPyYAMLをインストールする必要があります。

**例: YAMLファイルへの書き込み**

```python
import yaml

data = {'a list': [1, 42, 3.141, 1337, 'help', u'€'],
        'a string': 'boo!',
        'another dict': {'foo': 'bar', 'key': 'value', 'the answer': 42}}

with open('example.yaml', 'w') as f:
    yaml.dump(data, f, default_flow_style=False)

# これにより、データがYAML形式で構造化された`example.yaml`が作成されます。
```

**例: YAMLファイルからの読み込み**

```python
import yaml

with open('example.yaml', 'r') as f:
    data_loaded = yaml.safe_load(f)

print(data_loaded)

# 出力: 
# {'a list': [1, 42, 3.141, 1337, 'help', '€'],
#  'a string': 'boo!',
#  'another dict': {'foo': 'bar', 'key': 'value', 'the answer': 42}}
```

**構成管理にYAMLを使用する**

多くのプログラマーはYAMLをアプリケーションの設定管理に使用します。ここでは、設定ファイルをどのように構造化し、それを読み取るかの例です:

config.yaml:
```yaml
database:
  host: localhost
  port: 5432
  username: admin
  password: secret
```

Pythonで設定ファイルを読む:
```python
import yaml

with open('config.yaml', 'r') as f:
    config = yaml.safe_load(f)

print(config['database']['host'])  # 出力: localhost
```

**複雑な構造の取り扱い**

PyYAMLでは、カスタムPythonオブジェクトを定義することで複雑な構造を扱うことができます。ただし、任意の関数やオブジェクトを実行しないように`safe_load`を使用する安全な方法を確保してください。

```python
import yaml

# Pythonオブジェクトを定義
class Example:
    def __init__(self, value):
        self.value = value

# カスタムコンストラクタ
def constructor_example(loader, node):
    value = loader.construct_scalar(node)
    return Example(value)

# "!example" タグのためのコンストラクタを追加
yaml.add_constructor('!example', constructor_example)

yaml_str = "!example 'data'"
loaded = yaml.load(yaml_str, Loader=yaml.FullLoader)

print(loaded.value)  # 出力: data
```

このスニペットでは、`!example`はYAML文字列から値'data'で`Example`オブジェクトをインスタンス化するために使用されるカスタムタグです。このようなカスタムローダーは、より複雑なデータ構造やタイプの処理を可能にし、PyYAMLの柔軟性を拡大します。
