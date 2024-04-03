---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:44.413426-07:00
description: "\u65B9\u6CD5: JavaScript\u3067YAML\u3092\u6271\u3046\u5834\u5408\u3001\
  \u901A\u5E38\u3001JavaScript\u306B\u306FYAML\u306E\u7D44\u307F\u8FBC\u307F\u30D1\
  \u30FC\u30B5\u30FC\u304C\u542B\u307E\u308C\u3066\u3044\u306A\u3044\u305F\u3081\u3001\
  \u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\u306E\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\
  \u4F7F\u7528\u3059\u308B\u3053\u3068\u306B\u306A\u308A\u307E\u3059\u3002\u3053\u306E\
  \u76EE\u7684\u306E\u305F\u3081\u306B\u6700\u3082\u4EBA\u6C17\u306E\u3042\u308B\u30E9\
  \u30A4\u30D6\u30E9\u30EA\u306E\u4E00\u3064\u304C`js-yaml`\u3067\u3059\u3002`js-\u2026"
lastmod: '2024-03-13T22:44:42.702291-06:00'
model: gpt-4-0125-preview
summary: "JavaScript\u3067YAML\u3092\u6271\u3046\u5834\u5408\u3001\u901A\u5E38\u3001\
  JavaScript\u306B\u306FYAML\u306E\u7D44\u307F\u8FBC\u307F\u30D1\u30FC\u30B5\u30FC\
  \u304C\u542B\u307E\u308C\u3066\u3044\u306A\u3044\u305F\u3081\u3001\u30B5\u30FC\u30C9\
  \u30D1\u30FC\u30C6\u30A3\u306E\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u4F7F\u7528\u3059\
  \u308B\u3053\u3068\u306B\u306A\u308A\u307E\u3059\u3002\u3053\u306E\u76EE\u7684\u306E\
  \u305F\u3081\u306B\u6700\u3082\u4EBA\u6C17\u306E\u3042\u308B\u30E9\u30A4\u30D6\u30E9\
  \u30EA\u306E\u4E00\u3064\u304C`js-yaml`\u3067\u3059\u3002`js-yaml`\u3092\u4F7F\u7528\
  \u3057\u3066\u3001YAML\u3092JavaScript\u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\u306B\
  \u30D1\u30FC\u30B9\u3057\u305F\u308A\u3001\u305D\u306E\u9006\u3092\u884C\u3046\u3053\
  \u3068\u304C\u3067\u304D\u307E\u3059."
title: "YAML \u3092\u64CD\u4F5C\u3059\u308B"
weight: 41
---

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
