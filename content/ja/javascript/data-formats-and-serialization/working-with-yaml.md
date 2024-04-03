---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:44.413426-07:00
description: "YAML\u306F\u3001\u300CYAML Ain't Markup Language\u300D\u306E\u7565\u3067\
  \u3059\u3002\u3053\u308C\u306F\u3001\u4EBA\u304C\u8AAD\u307F\u3084\u3059\u3044\u30C7\
  \u30FC\u30BF\u76F4\u5217\u5316\u5F62\u5F0F\u3067\u3059\u3002JSON\u3084XML\u3068\u6BD4\
  \u3079\u3066\u5358\u7D14\u3067\u8AAD\u307F\u3084\u3059\u3044\u305F\u3081\u3001\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3057\u3070\u3057\u3070\u8A2D\u5B9A\u30D5\u30A1\
  \u30A4\u30EB\u3084\u8A00\u8A9E\u9593\u3067\u306E\u30C7\u30FC\u30BF\u4EA4\u63DB\u306B\
  \u3053\u308C\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.702291-06:00'
model: gpt-4-0125-preview
summary: "YAML\u306F\u3001\u300CYAML Ain't Markup Language\u300D\u306E\u7565\u3067\
  \u3059\u3002\u3053\u308C\u306F\u3001\u4EBA\u304C\u8AAD\u307F\u3084\u3059\u3044\u30C7\
  \u30FC\u30BF\u76F4\u5217\u5316\u5F62\u5F0F\u3067\u3059\u3002JSON\u3084XML\u3068\u6BD4\
  \u3079\u3066\u5358\u7D14\u3067\u8AAD\u307F\u3084\u3059\u3044\u305F\u3081\u3001\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3057\u3070\u3057\u3070\u8A2D\u5B9A\u30D5\u30A1\
  \u30A4\u30EB\u3084\u8A00\u8A9E\u9593\u3067\u306E\u30C7\u30FC\u30BF\u4EA4\u63DB\u306B\
  \u3053\u308C\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002."
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
