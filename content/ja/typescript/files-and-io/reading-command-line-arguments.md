---
date: 2024-01-20 17:57:08.107590-07:00
description: "How to: (\u3084\u308A\u65B9\uFF1A) TypeScript\u3067\u306F\u3001`process.argv`\u3092\
  \u4F7F\u3063\u3066\u5F15\u6570\u3092\u53D6\u308A\u307E\u3059\u3002\u7C21\u5358\u306A\
  \u4F8B\u3092\u898B\u3066\u307F\u307E\u3057\u3087\u3046\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:42.690723-06:00'
model: gpt-4-1106-preview
summary: "(\u3084\u308A\u65B9\uFF1A) TypeScript\u3067\u306F\u3001`process.argv`\u3092\
  \u4F7F\u3063\u3066\u5F15\u6570\u3092\u53D6\u308A\u307E\u3059\u3002\u7C21\u5358\u306A\
  \u4F8B\u3092\u898B\u3066\u307F\u307E\u3057\u3087\u3046\u3002"
title: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u306E\u8AAD\u307F\u53D6\
  \u308A"
weight: 23
---

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
