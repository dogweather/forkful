---
title:                "標準エラーへの書き込み"
html_title:           "Javascript: 標準エラーへの書き込み"
simple_title:         "標準エラーへの書き込み"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## なぜ

誰でもエラーを経験するものです。その時、コンソールにメッセージを表示するのではなく、裏でエラーを追跡するために、標準エラーに書き込むことが重要です。これはデバッグにも役立ち、チームメンバーとのコミュニケーションを円滑にするのに役立ちます。

## 使い方

```javascript
console.error("これは標準エラーに書き込まれます") 
```

出力：

```
これは標準エラーに書き込まれます
```

```javascript
console.error("エラーが発生しました", err) 
```

出力：

```
エラーが発生しました Error: サンプルエラー
```

## 深堀り

標準エラーに書き込むには、`console.error()`を使用します。このメソッドは、コンソールにメッセージを出力するのではなく、エラーオブジェクトを受け取ってエラーログを生成します。また、改行できないオブジェクトの場合、`console.error()`は`Object.prototype.toString`メソッドを使用してオブジェクトを文字列に変換し、それをエラーログとして表示します。

## See Also

- [標準エラーに書き込む方法](https://developer.mozilla.org/ja/docs/Web/API/Console/error)
- [エラーオブジェクト](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Error)