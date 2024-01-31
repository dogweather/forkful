---
title:                "YAMLを扱う"
date:                  2024-01-19
html_title:           "Bash: YAMLを扱う"
simple_title:         "YAMLを扱う"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
YAMLはデータのシリアライゼーション形式。設定ファイルやデータ交換で使われる。読みやすく、JSONより簡潔。

## How to: (どうやって？)
JavascriptでYAMLを扱うには`js-yaml`ライブラリを使う。`npm install js-yaml`でインストール。

```Javascript
const yaml = require('js-yaml');
const fs = require('fs');

// YAMLファイルの読み込み
const data = yaml.load(fs.readFileSync('example.yml', 'utf8'));

console.log(data);

// JavaScriptオブジェクトをYAMLに変換
const newYAML = yaml.dump({ title: 'New Item', value: 123 });
fs.writeFileSync('newExample.yml', newYAML, 'utf8');
```

YAMLデータを読み込んでJavaScriptオブジェクトにし、逆も可能。

## Deep Dive (深堀り)
YAMLは「YAML Ain't Markup Language」(再帰的頭字語)。2001年に登場。JSON, XMLの代わりに使えるが、可読性が特徴。大規模データではパフォーマンスに注意。

## See Also (関連する情報)
- YAML公式サイト: https://yaml.org
- `js-yaml`ライブラリ: https://github.com/nodeca/js-yaml
- JSONとYAMLの比較: https://www.atatus.com/blog/yaml-vs-json/
