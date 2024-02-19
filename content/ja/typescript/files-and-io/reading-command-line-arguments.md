---
aliases:
- /ja/typescript/reading-command-line-arguments/
date: 2024-01-20 17:57:08.107590-07:00
description: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u3092\u8AAD\u3080\
  \u3068\u306F\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\u304C\u5B9F\u884C\u6642\u306B\u8FFD\
  \u52A0\u60C5\u5831\u3092\u53D7\u3051\u53D6\u308B\u3053\u3068\u3067\u3059\u3002\u3053\
  \u308C\u306B\u3088\u308A\u3001\u30E6\u30FC\u30B6\u30FC\u304C\u52D5\u7684\u306B\u6319\
  \u52D5\u3092\u5909\u66F4\u3067\u304D\u308B\u3088\u3046\u306B\u306A\u308A\u307E\u3059\
  \u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:54.696573
model: gpt-4-1106-preview
summary: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u3092\u8AAD\u3080\
  \u3068\u306F\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\u304C\u5B9F\u884C\u6642\u306B\u8FFD\
  \u52A0\u60C5\u5831\u3092\u53D7\u3051\u53D6\u308B\u3053\u3068\u3067\u3059\u3002\u3053\
  \u308C\u306B\u3088\u308A\u3001\u30E6\u30FC\u30B6\u30FC\u304C\u52D5\u7684\u306B\u6319\
  \u52D5\u3092\u5909\u66F4\u3067\u304D\u308B\u3088\u3046\u306B\u306A\u308A\u307E\u3059\
  \u3002"
title: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u306E\u8AAD\u307F\u53D6\
  \u308A"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
コマンドライン引数を読むとは、プログラムが実行時に追加情報を受け取ることです。これにより、ユーザーが動的に挙動を変更できるようになります。

## How to: (やり方：)
TypeScriptでは、`process.argv`を使って引数を取ります。簡単な例を見てみましょう。

```TypeScript
// hello.ts

// コマンドライン引数を取得
const args = process.argv.slice(2);

// 引数があれば、それを使って挨拶
console.log(`こんにちは、${args[0] ? args[0] : 'みなさん'}！`);
```

これを実行するには、以下をターミナルに打ち込みます:

```bash
tsc hello.ts && node hello.js 太郎
```

出力:

```
こんにちは、太郎！
```

## Deep Dive (深掘り：)
コマンドライン引数の読み込みはUNIX時代からあります。`process.argv`はNode.js環境で提供されるため、ブラウザ上のTypeScriptでは使えません。より複雑な引数やオプションの管理には、`commander`や`yargs`などのサードパーティライブラリを利用すると便利です。

`process.argv`は全ての引数を文字列として取得するため、数値などに変換する必要があります。また、Node.jsでは`process.argv[0]`はnodeのパス、`process.argv[1]`は実行しているスクリプトのパスが格納されており、実際の引数はインデックス2から始まります。

## See Also (関連情報：)
- Node.js documentation on process.argv: https://nodejs.org/docs/latest/api/process.html#process_process_argv
- npm package `commander`: https://www.npmjs.com/package/commander
- npm package `yargs`: https://www.npmjs.com/package/yargs
