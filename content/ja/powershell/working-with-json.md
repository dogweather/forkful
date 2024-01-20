---
title:                "「JSONを扱う」"
html_title:           "PowerShell: 「JSONを扱う」"
simple_title:         "「JSONを扱う」"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/working-with-json.md"
---

{{< edit_this_page >}}

# それは何で、なぜ？
JSONを扱うとはどういうことか、そして何故プログラマーがそれを行うのかについて、2〜3文で説明します。

JSONとは、テキスト形式でデータを表現するためのフォーマットです。プログラマーは、プログラム内でデータを簡単に読み書きするために、JSONを使用します。

## 方法：
次のように、JSONをPowerShellで作業する方法をお見せします。

```PowerShell
# テキストをJSON形式に変換する
$text = 'hello world'
$json = $text | ConvertTo-Json

# JSONオブジェクトをテキストに変換する
$json | ConvertFrom-Json

# ファイルからJSONを読み取る
$json = Get-Content 'file.json' | ConvertFrom-Json
```

## 深堀り
### 歴史的背景
JSONは、プログラミング言語JavaScriptのオブジェクト表記から着想を得て、ダグラス・クロックフォード氏によって生み出されました。JavaScript以外のプログラミング言語でも利用可能で、WebアプリケーションやAPI管理において、デファクトスタンダードとなっています。

### 代替手段
JSON以外にもデータのフォーマットとして、XMLやCSVがあります。しかし、XMLは煩雑な書式であり、CSVは構造化されたデータを表現するには適していません。そのため、多くのプログラマーがJSONを選択する傾向にあります。

### 実装の詳細
PowerShellには、JSONを操作するための`ConvertTo-Json`、`ConvertFrom-Json`などのコマンドが備わっています。また、外部ライブラリを使用することでより高度な操作が可能になります。

## 関連情報を見る
以下のリンクから、JSONについてさらに学ぶことができます。

- [Wikipedia - JSON](https://ja.wikipedia.org/wiki/JavaScript_Object_Notation)
- [JSON.org](https://www.json.org/)