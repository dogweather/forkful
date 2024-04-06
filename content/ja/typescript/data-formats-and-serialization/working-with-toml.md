---
date: 2024-01-26 04:27:31.888438-07:00
description: "\u65B9\u6CD5\uFF1A \u307E\u305A\u3001TOML\u30D1\u30FC\u30B5\u30FC\u304C\
  \u5FC5\u8981\u306B\u306A\u308A\u307E\u3059\u3002`@iarna/toml`\u306F\u4EBA\u6C17\u306E\
  \u9078\u629E\u80A2\u3067\u3059\u3002npm\u3067\u30A4\u30F3\u30B9\u30C8\u30FC\u30EB\
  \u3057\u307E\u3059\uFF1A`npm install @iarna/toml --save`\u3002\u6B21\u306B\u3001\
  TOML\u30D5\u30A1\u30A4\u30EB\u3092\u8AAD\u307F\u8FBC\u307F\u3001JavaScript\u30AA\
  \u30D6\u30B8\u30A7\u30AF\u30C8\u306B\u89E3\u6790\u3059\u308B\u65B9\u6CD5\u3067\u3059\
  \uFF1A."
lastmod: '2024-04-05T22:37:50.085298-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A \u307E\u305A\u3001TOML\u30D1\u30FC\u30B5\u30FC\u304C\u5FC5\
  \u8981\u306B\u306A\u308A\u307E\u3059\u3002`@iarna/toml`\u306F\u4EBA\u6C17\u306E\u9078\
  \u629E\u80A2\u3067\u3059\u3002npm\u3067\u30A4\u30F3\u30B9\u30C8\u30FC\u30EB\u3057\
  \u307E\u3059\uFF1A`npm install @iarna/toml --save`\u3002\u6B21\u306B\u3001TOML\u30D5\
  \u30A1\u30A4\u30EB\u3092\u8AAD\u307F\u8FBC\u307F\u3001JavaScript\u30AA\u30D6\u30B8\
  \u30A7\u30AF\u30C8\u306B\u89E3\u6790\u3059\u308B\u65B9\u6CD5\u3067\u3059\uFF1A."
title: "TOML\u3092\u6271\u3046\u65B9\u6CD5"
weight: 39
---

## 方法：
まず、TOMLパーサーが必要になります。`@iarna/toml`は人気の選択肢です。npmでインストールします：`npm install @iarna/toml --save`。次に、TOMLファイルを読み込み、JavaScriptオブジェクトに解析する方法です：

```typescript
import * as fs from 'fs';
import toml from '@iarna/toml';

const tomlContent = fs.readFileSync('config.toml', 'utf-8');
const parsedData = toml.parse(tomlContent);

console.log(parsedData);
```
もし`config.toml`が以下を含むならば：
```
[server]
port = 8080
```
出力は：
```
{ server: { port: 8080 } }
```
となります。そして、TOMLファイルに書き込むのも同様に簡単です：
```typescript
import * as fs from 'fs';
import { stringify } from '@iarna/toml';

const obj = { server: { port: 8080 } };
const tomlString = stringify(obj);
fs.writeFileSync('config.toml', tomlString);
``` 
このコードを実行すると、`config.toml`にオブジェクトがTOML形式で書き込まれます。

## 深く掘り下げる
TOMLは、GitHubの共同創設者であるTom Preston-Wernerによって、2013年頃にINIやYAMLなどの他の形式の制限を感じたことに対する反応として作成されました。それは、データ構造に簡単に解析できるように明確であることを目指して設計されており、したがって、設定ファイルに好まれます。例えば、JSONにはコメントがなく、YAMLはより複雑です。TOMLは、そのシンプルさと複雑なデータ階層を明確に表現できる能力で際立っています。

内部的には、TypeScriptでTOMLを解析するとき、テキストデータを言語が操作できる構造化された形式に変換しています。これには、字句解析（生のテキストをトークンに変換すること）と解析（内部データ構造を構築すること）が関係しており、`@iarna/toml`はそれらをシームレスに処理します。絵文字のサポートは、TOMLのユーザー中心のアプローチを示す楽しいタッチです。

## 参照
- TOML公式仕様：https://toml.io/en/
- `@iarna/toml`パッケージ：https://www.npmjs.com/package/@iarna/toml
- TOML、YAML、JSONの比較：https://blog.bitsrc.io/choosing-the-right-configuration-file-format-toml-vs-yaml-vs-json-71b5be8968ea
