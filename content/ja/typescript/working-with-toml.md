---
title:                "TOMLを扱う方法"
date:                  2024-01-26T04:27:31.888438-07:00
model:                 gpt-4-0125-preview
simple_title:         "TOMLを扱う方法"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/working-with-toml.md"
---

{{< edit_this_page >}}

## 何となぜ？
TOMLは、Tom's Obvious, Minimal Languageの略で、JSONやYAMLに似たデータシリアライゼーション形式です。プログラマーは、その人間が読みやすく、データ型に直接的にマッピングすることができるため、設定ファイルやデータ交換のための第一選択肢として使用します。

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