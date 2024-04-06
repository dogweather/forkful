---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:59.159775-07:00
description: "\u3069\u306E\u3088\u3046\u306B\u3057\u3066\uFF1A TypeScript\u3067YAML\u3092\
  \u6271\u3046\u5834\u5408\u3001\u901A\u5E38\u306FYAML\u5185\u5BB9\u3092JavaScript\u30AA\
  \u30D6\u30B8\u30A7\u30AF\u30C8\u306B\u89E3\u6790\u3057\u3001\u5834\u5408\u306B\u3088\
  \u3063\u3066\u306FJavaScript\u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\u3092YAML\u306B\
  \u623B\u3059\u3053\u3068\u304C\u95A2\u308F\u3063\u3066\u304D\u307E\u3059\u3002\u3053\
  \u308C\u306B\u306F\u30D1\u30FC\u30B5\u30FC\u304C\u5FC5\u8981\u3067\u3001`js-\u2026"
lastmod: '2024-04-05T21:53:42.697364-06:00'
model: gpt-4-0125-preview
summary: ''
title: "YAML \u3092\u64CD\u4F5C\u3059\u308B"
weight: 41
---

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
