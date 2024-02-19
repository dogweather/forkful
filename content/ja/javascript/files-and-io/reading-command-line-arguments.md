---
aliases:
- /ja/javascript/reading-command-line-arguments/
date: 2024-01-20 17:56:18.655594-07:00
description: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u3092\u8AAD\u3080\
  \u3063\u3066\u3044\u3046\u306E\u306F\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\u3092\u958B\
  \u59CB\u3059\u308B\u3068\u304D\u306B\u8FFD\u52A0\u60C5\u5831\u3092\u4E0E\u3048\u308B\
  \u65B9\u6CD5\u3067\u3059\u3002\u306A\u305C\u3084\u308B\u304B\u3068\u3044\u3046\u3068\
  \u3001\u30B9\u30AF\u30EA\u30D7\u30C8\u306E\u632F\u308B\u821E\u3044\u3092\u5B9F\u884C\
  \u6642\u306B\u67D4\u8EDF\u306B\u5909\u66F4\u3057\u305F\u3044\u304B\u3089\u3067\u3059\
  \u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:55.283188
model: gpt-4-1106-preview
summary: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u3092\u8AAD\u3080\
  \u3063\u3066\u3044\u3046\u306E\u306F\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\u3092\u958B\
  \u59CB\u3059\u308B\u3068\u304D\u306B\u8FFD\u52A0\u60C5\u5831\u3092\u4E0E\u3048\u308B\
  \u65B9\u6CD5\u3067\u3059\u3002\u306A\u305C\u3084\u308B\u304B\u3068\u3044\u3046\u3068\
  \u3001\u30B9\u30AF\u30EA\u30D7\u30C8\u306E\u632F\u308B\u821E\u3044\u3092\u5B9F\u884C\
  \u6642\u306B\u67D4\u8EDF\u306B\u5909\u66F4\u3057\u305F\u3044\u304B\u3089\u3067\u3059\
  \u3002"
title: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u306E\u8AAD\u307F\u53D6\
  \u308A"
---

{{< edit_this_page >}}

## What & Why? / 何となぜ？
コマンドライン引数を読むっていうのは、プログラムを開始するときに追加情報を与える方法です。なぜやるかというと、スクリプトの振る舞いを実行時に柔軟に変更したいからです。

## How to / 方法
Node.jsでコマンドライン引数を読むのは`process.argv`を使います。これは引数を配列として保持します。例を見てみましょう。

```javascript
// process_argv_example.js
// 引数をコンソールに出力します
process.argv.forEach((val, index) => {
  console.log(`${index}: ${val}`);
});

// 実行例: node process_argv_example.js one two=2 three=3
// 出力:
// 0: /path/to/node
// 1: /path/to/process_argv_example.js
// 2: one
// 3: two=2
// 4: three=3
```

引数はゼロから数えて、最初の二つはNodeのパスとスクリプトのパスです。自分たちの引数は二から始まります。

## Deep Dive / 深掘り
コマンドライン引数はUNIX時代からあります。古くからプログラムの振る舞いをコントロールする手段です。Node.jsでは`process.argv`を使う以外に、より高度なパッケージもあります。例えば、`yargs`や`commander.js`など。「引数をパースする」っていう作業は、「フラグ」や「オプション」に変えることです。これらパッケージは、その作業を簡単にしてくれます。

例: `commander.js`
```javascript
const { program } = require('commander');

program
  .option('-d, --debug', 'output extra debugging')
  .parse(process.argv);

if (program.debug) {
  console.log('Debugging is on.');
}
// 実行例: node script.js -d
// 出力: Debugging is on.
```

Node.js以外にも、他のプログラミング言語には同様の概念が存在します。しかし、実装や使いやすさは言語によって異なります。

## See Also / 関連リンク
- Node.js Documentation for `process.argv`: [https://nodejs.org/docs/latest/api/process.html#process_process_argv](https://nodejs.org/docs/latest/api/process.html#process_process_argv)
- npm package `yargs`: [https://www.npmjs.com/package/yargs](https://www.npmjs.com/package/yargs)
- npm package `commander`: [https://www.npmjs.com/package/commander](https://www.npmjs.com/package/commander)
