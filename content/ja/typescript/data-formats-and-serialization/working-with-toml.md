---
date: 2024-01-26 04:27:31.888438-07:00
description: "TOML\u306F\u3001Tom's Obvious, Minimal Language\u306E\u7565\u3067\u3001\
  JSON\u3084YAML\u306B\u4F3C\u305F\u30C7\u30FC\u30BF\u30B7\u30EA\u30A2\u30E9\u30A4\
  \u30BC\u30FC\u30B7\u30E7\u30F3\u5F62\u5F0F\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u306F\u3001\u305D\u306E\u4EBA\u9593\u304C\u8AAD\u307F\u3084\u3059\u304F\
  \u3001\u30C7\u30FC\u30BF\u578B\u306B\u76F4\u63A5\u7684\u306B\u30DE\u30C3\u30D4\u30F3\
  \u30B0\u3059\u308B\u3053\u3068\u304C\u3067\u304D\u308B\u305F\u3081\u3001\u8A2D\u5B9A\
  \u30D5\u30A1\u30A4\u30EB\u3084\u30C7\u30FC\u30BF\u4EA4\u63DB\u306E\u305F\u3081\u306E\
  \u7B2C\u4E00\u9078\u629E\u80A2\u3068\u3057\u3066\u4F7F\u7528\u3057\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:41.791112-06:00'
model: gpt-4-0125-preview
summary: "TOML\u306F\u3001Tom's Obvious, Minimal Language\u306E\u7565\u3067\u3001\
  JSON\u3084YAML\u306B\u4F3C\u305F\u30C7\u30FC\u30BF\u30B7\u30EA\u30A2\u30E9\u30A4\
  \u30BC\u30FC\u30B7\u30E7\u30F3\u5F62\u5F0F\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u306F\u3001\u305D\u306E\u4EBA\u9593\u304C\u8AAD\u307F\u3084\u3059\u304F\
  \u3001\u30C7\u30FC\u30BF\u578B\u306B\u76F4\u63A5\u7684\u306B\u30DE\u30C3\u30D4\u30F3\
  \u30B0\u3059\u308B\u3053\u3068\u304C\u3067\u304D\u308B\u305F\u3081\u3001\u8A2D\u5B9A\
  \u30D5\u30A1\u30A4\u30EB\u3084\u30C7\u30FC\u30BF\u4EA4\u63DB\u306E\u305F\u3081\u306E\
  \u7B2C\u4E00\u9078\u629E\u80A2\u3068\u3057\u3066\u4F7F\u7528\u3057\u307E\u3059\u3002\
  ."
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
