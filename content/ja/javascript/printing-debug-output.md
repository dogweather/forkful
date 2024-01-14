---
title:    "Javascript: デバッグ出力のプリント"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

## なぜデバッグ出力をプリントするのか

デバッグ出力をプリントすることによって、コードの実行を追跡することができ、問題を特定して修正することができます。

## プリントの方法

```Javascript
function calculateSum(num1, num2) {
    console.log("引数: " + num1 + ", " + num2);
    let sum = num1 + num2;
    console.log("合計: " + sum);
    return sum;
}

calculateSum(5, 3);
```

出力:

```
引数: 5, 3
合計: 8
```

## プリントの深層

デバッグ出力をプリントすることによって、コードの実行中に発生したエラーや予期しない動作を特定することができます。プリントする情報を適切に選択することで、効率的にバグを修正できます。また、デバッグ出力をプリントすることで、他の開発者とコードの挙動を共有することもできます。

## 参考リンク

[デバッグ出力のプリント方法（英語）](https://www.w3schools.com/jsref/met_console_log.asp)

[JavaScriptのデバッグ方法（英語）](https://blog.teamtreehouse.com/how-to-debug-javascript-with-chrome-developer-tools)

[デバッグのベストプラクティス（英語）](https://stackify.com/getting-started-with-debugging-javascript/)