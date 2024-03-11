---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:59.159775-07:00
description: "\u2026"
lastmod: '2024-03-11T00:14:15.385195-06:00'
model: gpt-4-0125-preview
summary: "\u2026"
title: "YAML \u3092\u64CD\u4F5C\u3059\u308B"
---

{{< edit_this_page >}}

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
