---
date: 2024-01-20 17:56:18.655594-07:00
description: "How to / \u65B9\u6CD5 Node.js\u3067\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\
  \u30F3\u5F15\u6570\u3092\u8AAD\u3080\u306E\u306F`process.argv`\u3092\u4F7F\u3044\
  \u307E\u3059\u3002\u3053\u308C\u306F\u5F15\u6570\u3092\u914D\u5217\u3068\u3057\u3066\
  \u4FDD\u6301\u3057\u307E\u3059\u3002\u4F8B\u3092\u898B\u3066\u307F\u307E\u3057\u3087\
  \u3046\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.483822-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u306E\u8AAD\u307F\u53D6\
  \u308A"
weight: 23
---

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
