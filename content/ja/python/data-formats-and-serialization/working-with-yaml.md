---
title:                "YAML を操作する"
date:                  2024-02-03T19:26:33.277206-07:00
model:                 gpt-4-0125-preview
simple_title:         "YAML を操作する"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ?
YAMLは "YAML Ain't Markup Language" の略で、人間が読めるデータ直列化形式です。プログラマーはそのシンプルな構文と、XMLやJSONのような他の形式と比べて読みやすさのために、設定ファイル、プロセス間メッセージング、およびデータストレージにYAMLを使用します。

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
