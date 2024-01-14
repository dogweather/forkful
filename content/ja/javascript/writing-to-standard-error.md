---
title:    "Javascript: 標準エラーに書き込むこと"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## なぜ
プログラムを書く際、標準エラー出力に書き込む理由は何でしょうか？ 

プログラムで何かが間違ったり、バグが発生した場合、エラーメッセージを標準エラー出力に書き込むことでユーザーに正しい情報を提供できます。また、標準出力と標準エラー出力を区別することでデバッグやログ出力が容易になります。 

## 方法
標準エラー出力に書き込む方法は、プログラミング言語によって異なりますが、大多数の言語で以下のように記述できます。 

```Javascript
console.error("エラーメッセージ");
```

上記の例では、JavaScriptでコンソールにエラーメッセージを出力する方法を示しています。実際にプログラムを実行してみると、以下のような出力が得られます。 

```Javascript
>> console.error("エラーメッセージ");
エラーメッセージ
```

## ディープダイブ
標準エラー出力に書き込む際の注意点として、エラーメッセージを標準エラー出力に書き込む際には適切なエスケープ処理を行う必要があります。これにより、プログラムが正しく動作しない場合でも意図せずセキュリティ上の問題を引き起こすことがなくなります。 

また、標準エラー出力は通常画面に出力されず、ログファイルなどに書き込まれるため、デバッグやバグ修正を行う際には重要な情報源となります。適切なエラーメッセージを書き込むことで、バグの原因を特定するのに役立ちます。 

## See Also
- [console.error() - MDN Web Docs](https://developer.mozilla.org/ja/docs/Web/API/Console/error)
- [標準エラー出力 - Wikipedia](https://ja.wikipedia.org/wiki/%E6%A8%99%E6%BA%96%E3%82%A8%E3%83%A9%E3%83%BC%E5%87%BA%E5%8A%9B)