---
title:                "標準エラーへの書き込み"
date:                  2024-01-19
html_title:           "Arduino: 標準エラーへの書き込み"
simple_title:         "標準エラーへの書き込み"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
標準エラーへの書き出しは、プログラムのエラーメッセージを出力する方法です。デバックやログとしてエラー情報を分けておくと、問題解析がしやすくなるから使います。

## How to: (方法)
```javascript
// 標準エラーにメッセージを書き出す例
console.error('エラー: 不正な操作を検出しました。');

// 出力例（コンソールに表示されるエラーメッセージ）
// エラー: 不正な操作を検出しました。
```

## Deep Dive (詳細情報)
かつて、コンピュータプログラムはコマンドラインから実行されていたため標準出力（stdout）と標準エラー（stderr）が分けられました。デバッグ情報やエラーメッセージはstderrに出力し、プログラムの通常の出力結果はstdoutに出します。JavaScriptでは`console.error()`を使うことが一般的です。ストリームを直接扱う方法もありますが、`process.stderr.write()`はNode.jsの環境で用いられます。

## See Also (関連情報)
- MDN Web Docs (`console.error`): https://developer.mozilla.org/en-US/docs/Web/API/Console/error
- Node.js Documentation (`process.stderr`): https://nodejs.org/api/process.html#process_process_stderr
