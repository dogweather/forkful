---
title:                "コマンドライン引数の読み取り"
date:                  2024-01-20T17:56:18.655594-07:00
model:                 gpt-4-1106-preview
simple_title:         "コマンドライン引数の読み取り"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/reading-command-line-arguments.md"
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
