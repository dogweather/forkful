---
title:                "YAML を操作する"
aliases:
- /ja/javascript/working-with-yaml.md
date:                  2024-02-03T19:25:44.413426-07:00
model:                 gpt-4-0125-preview
simple_title:         "YAML を操作する"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？

YAMLは、「YAML Ain't Markup Language」の略です。これは、人が読みやすいデータ直列化形式です。JSONやXMLと比べて単純で読みやすいため、プログラマーはしばしば設定ファイルや言語間でのデータ交換にこれを使用します。

## 方法:

JavaScriptでYAMLを扱う場合、通常、JavaScriptにはYAMLの組み込みパーサーが含まれていないため、サードパーティのライブラリを使用することになります。この目的のために最も人気のあるライブラリの一つが`js-yaml`です。`js-yaml`を使用して、YAMLをJavaScriptオブジェクトにパースしたり、その逆を行うことができます。

まず、`js-yaml`をインストールする必要があります：

```bash
npm install js-yaml
```

その後、プロジェクトで使用できます。YAMLファイルを読み込み、それをJavaScriptオブジェクトにパースする方法は以下の通りです：

```javascript
// js-yamlモジュールをrequireする
const yaml = require('js-yaml');
const fs   = require('fs');

// ファイルからYAMLを読み込む
try {
  const doc = yaml.load(fs.readFileSync('./config.yaml', 'utf8'));
  console.log(doc);
} catch (e) {
  console.error(e);
}
```

もし`config.yaml`ファイルがこのようになっている場合：

```yaml
version: 1
services:
  web:
    image: "myapp/web:latest"
    ports:
      - "5000:5000"
```

出力はこうなります：

```javascript
{ version: 1,
  services: 
   { web: 
      { image: 'myapp/web:latest',
        ports: [ '5000:5000' ] } } }
```

逆に、JavaScriptオブジェクトをYAML文字列に変換するには：

```javascript
const yaml = require('js-yaml');
const obj = {
  version: 1,
  services: {
    web: {
      image: "myapp/web:latest",
      ports: ["5000:5000"]
    }
  }
};

const yamlStr = yaml.dump(obj);
console.log(yamlStr);
```

このコードはこんな出力を生成します：

```yaml
version: 1
services:
  web:
    image: myapp/web:latest
    ports:
      - '5000:5000'
```

`js-yaml`を使用すると、YAMLのパースと直列化をJavaScriptプロジェクトに簡単に統合し、データの交換性と設定管理を強化することができます。
