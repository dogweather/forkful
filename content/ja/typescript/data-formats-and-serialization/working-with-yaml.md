---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:59.159775-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:41.788367-06:00'
model: gpt-4-0125-preview
summary: "YAML\u306F\u3001\u4EBA\u9593\u304C\u8AAD\u307F\u3084\u3059\u3044\u3053\u3068\
  \u3092\u76EE\u7684\u3068\u3057\u3066\u8A2D\u8A08\u3055\u308C\u305F\u30C7\u30FC\u30BF\
  \u76F4\u5217\u5316\u8A00\u8A9E\u3067\u3042\u308A\u3001\u8A2D\u5B9A\u30D5\u30A1\u30A4\
  \u30EB\u3001\u30D7\u30ED\u30BB\u30B9\u9593\u30E1\u30C3\u30BB\u30FC\u30B8\u30F3\u30B0\
  \u3001\u30C7\u30FC\u30BF\u30B9\u30C8\u30EC\u30FC\u30B8\u306B\u3088\u304F\u4F7F\u7528\
  \u3055\u308C\u307E\u3059\u3002YAML\u306F\u305D\u306E\u8AAD\u307F\u3084\u3059\u3055\
  \u3068\u4F7F\u3044\u3084\u3059\u3055\u304B\u3089\u3001\u7279\u306B\u8907\u96D1\u306A\
  \u69CB\u9020\u30C7\u30FC\u30BF\u3092\u6271\u3046\u5834\u5408\u306B\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u30FC\u306B\u597D\u307E\u308C\u3001TypeScript\u3067\u958B\u767A\u3055\
  \u308C\u305F\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u306B\u3068\u3063\u3066\
  \u512A\u308C\u305F\u9078\u629E\u3068\u306A\u308A\u307E\u3059\u3002."
title: "YAML \u3092\u64CD\u4F5C\u3059\u308B"
weight: 41
---

## 何となぜ？
YAMLは、人間が読みやすいことを目的として設計されたデータ直列化言語であり、設定ファイル、プロセス間メッセージング、データストレージによく使用されます。YAMLはその読みやすさと使いやすさから、特に複雑な構造データを扱う場合にプログラマーに好まれ、TypeScriptで開発されたアプリケーションにとって優れた選択となります。

## どのようにして：
TypeScriptでYAMLを扱う場合、通常はYAML内容をJavaScriptオブジェクトに解析し、場合によってはJavaScriptオブジェクトをYAMLに戻すことが関わってきます。これにはパーサーが必要で、`js-yaml`はTypeScriptプロジェクトに簡単に統合できる人気のある選択肢です。

### js-yamlをインストールする
まず、`js-yaml`をプロジェクトに追加します：

```bash
npm install js-yaml
```

### YAMLをJavaScriptオブジェクトに解析する
以下の内容を持つYAMLファイル`config.yaml`があるとします：

```yaml
database:
  host: localhost
  port: 5432
  username: user
  password: pass
```

このファイルをJavaScriptオブジェクトとして読み込み、解析するには以下のようにします：

```typescript
import * as fs from 'fs';
import * as yaml from 'js-yaml';

// YAMLファイルを読み込み、解析する
const fileContents = fs.readFileSync('./config.yaml', 'utf8');
const data = yaml.load(fileContents) as Record<string, any>;

console.log(data);
```

**サンプル出力:**

```json
{
  "database": {
    "host": "localhost",
    "port": 5432,
    "username": "user",
    "password": "pass"
  }
}
```

### JavaScriptオブジェクトをYAMLに変換する
逆に、JavaScriptオブジェクトをYAML文字列に変換する必要がある場合は、次のように`js-yaml`を使用できます：

```typescript
import * as yaml from 'js-yaml';

const obj = {
  title: "Example",
  is_published: true,
  author: {
    name: "Jane Doe",
    age: 34
  }
};

const yamlStr = yaml.dump(obj);
console.log(yamlStr);
```

**サンプル出力:**

```yaml
title: Example
is_published: true
author:
  name: Jane Doe
  age: 34
```

このスニペットは、JavaScriptオブジェクトをYAML文字列に変換し、出力します。実際には、これをファイルに書き戻したり、アプリケーションの他の部分で使用したりするかもしれません。
