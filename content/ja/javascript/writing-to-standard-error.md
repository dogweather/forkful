---
title:                "Javascript: 「標準エラーへの書き込み」"
simple_title:         "「標準エラーへの書き込み」"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## なぜ

プログラミングでは、デバッグが非常に重要です。プログラムが正しく動作しないとき、エラーメッセージは開発者にとって非常に役立つ情報源となります。その中でも、標準エラー出力に書き込むことは、最も重要な手段の一つです。

## 方法

以下のコード例を見てみましょう。例えば、ある関数が引数に数値を取るとき、その数値が負の場合にエラーメッセージを標準エラー出力に書き込むことを考えます。

```Javascript
const square = (num) => {
  if (num < 0) {
    console.error("数値は正でなければいけません");
    return;
  }
  return num * num;
}

console.log(square(2)); // 4
console.log(square(-2)); // undefined
```

このように、`console.error`を使用することで、関数の動作が想定と異なる場合に開発者に対して警告を出すことができます。また、`console.error`は場所を特定しやすいため、デバッグの際にも非常に役立ちます。

## 深堀り

標準エラー出力に書き込まれるメッセージは、通常標準出力に書き込まれるメッセージとは別のファイルに保存されます。それぞれの出力は異なるストリームを使用しており、標準出力は`process.stdout`、標準エラー出力は`process.stderr`を使用します。

また、`console.error`は単にエラーメッセージを書き込むだけではなく、エラーオブジェクトを受け取って詳細な情報を出力することもできます。

## 併せて見ておきたい

- [Node.jsの標準エラー出力について](https://nodejs.org/api/console.html#console_console_error_data_args)
- [Javascriptのエラーオブジェクトについて](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Error)