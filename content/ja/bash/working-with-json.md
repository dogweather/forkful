---
title:                "JSONを扱う方法"
date:                  2024-01-19
html_title:           "Arduino: JSONを扱う方法"
simple_title:         "JSONを扱う方法"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSONはデータフォーマットです。プログラマはデータ交換や設定ファイルに使います。コンパクトで人間にも読みやすく、多くのプログラミング言語で容易に扱えます。

## How to:
BashでJSONを扱う一般的なツールには`jq`があります。例えば、`jq`を使ってJSONから特定の値を取得するには：

```Bash
echo '{"name": "Taro", "age": 30}' | jq '.name'
```

出力：

```Bash
"Taro"
```

配列から特定要素を取り出すには：

```Bash
echo '{"users": [{"name": "Taro"}, {"name": "Hanako"}]}' | jq '.users[1].name'
```

出力：

```Bash
"Hanako"
```

## Deep Dive
JSON（JavaScript Object Notation）は2001年に登場しました。代替のフォーマットにはXMLやYAMLがあります。`jq`はC言語で記述されており、速度と柔軟性のバランスが良いツールです。

## See Also
- `jq`の公式ドキュメント：https://stedolan.github.io/jq/manual/
- JSONの仕様：https://www.json.org/json-en.html
- `jq`の処理例集：https://jqplay.org/
