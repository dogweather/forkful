---
title:                "標準エラーへの書き込み"
date:                  2024-01-19
html_title:           "Arduino: 標準エラーへの書き込み"
simple_title:         "標準エラーへの書き込み"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)

標準エラーへの書き込みは、プログラムのエラーメッセージや診断を出力する方式です。これを行う理由は、エラー情報を標準出力（ログやユーザーへの通常の出力）から分離して、問題の解析やデバッグを容易にするためです。

## How to: (方法)

```typescript
// 標準エラーにメッセージを書き込むシンプルな方法
console.error('エラーが発生しました。');

// 標準エラーを直接使うより洗練された方法
process.stderr.write('詳細なエラー情報。\n');
```

サンプル出力（コンソール）：

```
エラーが発生しました。
詳細なエラー情報。
```

## Deep Dive (深い潜水)

標準エラーはUNIX時代から存在し、プログラムとユーザ間でエラーメッセージを分ける伝統的な方法です。`console.error`は簡単で直感的な方法ですが、`process.stderr.write`はNode.jsに特有な、より直接的な書き込み方法です。また、ストリームを使って非同期に書き込みを行うこともできます。標準出力とは異なり、バッファリングされないためエラー情報が即座にユーザーに届けられます。

## See Also (関連情報)

- [Node.js公式ドキュメント](https://nodejs.org/api/process.html#process_process_stderr)
- [Console API リファレンス](https://developer.mozilla.org/ja/docs/Web/API/console)
