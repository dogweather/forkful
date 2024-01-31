---
title:                "コマンドライン引数の読み取り"
date:                  2024-01-20T17:57:08.107590-07:00
model:                 gpt-4-1106-preview
simple_title:         "コマンドライン引数の読み取り"

category:             "TypeScript"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/reading-command-line-arguments.md"
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
