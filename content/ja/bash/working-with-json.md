---
title:                "「JSONを使ったプログラミング」"
html_title:           "Bash: 「JSONを使ったプログラミング」"
simple_title:         "「JSONを使ったプログラミング」"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/working-with-json.md"
---

{{< edit_this_page >}}

## なぜ

JSONは、現代のウェブアプリケーションやAPIで非常に一般的に使用されるデータフォーマットです。JSONを扱うことで、より柔軟で効率的なデータ処理が可能になります。

## 使い方

JSONをBashで扱う方法は非常に簡単です。以下の例を参考にしてください。

### JSONオブジェクトの作成

```
$ json='{ "name": "John", "age": 25, "city": "Tokyo" }'
```

### 属性の取得

```
$ name=$(echo $json | jq -r '.name') # "John"
$ age=$(echo $json | jq -r '.age') # 25
$ city=$(echo $json | jq -r '.city') # "Tokyo"
```

### 属性の追加

```
$ new_json=$(echo $json | jq '.+.{"country": "Japan"}') # { "name": "John", "age": 25, "city": "Tokyo", "country": "Japan" }
```

### ネストされたオブジェクトの取得

```
$ location=$(echo $json | jq -r '.location | "\(.city), \(.country)"') # "Tokyo, Japan"
```

## ディープダイブ

Bashには、JSONを処理するための組み込みコマンドやツールがありません。そのため、jqという外部のツールを使用することでJSONを扱うことができます。jqは、コマンドラインからJSONを操作するための強力なツールであり、Bashスクリプトでの使用に適しています。

また、JSONの配列や複雑なネスト構造を扱う場合は、より複雑なjqのコマンドを使用する必要があります。また、jqのドキュメントを参考にすることで、より高度なJSONの操作も可能です。

## その他参考リンク

- [BashでJSONを処理する方法](https://dev.classmethod.jp/articles/bash-json/)
- [jqドキュメント](https://stedolan.github.io/jq/manual/)