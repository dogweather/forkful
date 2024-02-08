---
title:                "デバッガーの使い方"
aliases:
- ja/javascript/using-a-debugger.md
date:                  2024-01-26T03:50:16.985448-07:00
model:                 gpt-4-0125-preview
simple_title:         "デバッガーの使い方"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/using-a-debugger.md"
---

{{< edit_this_page >}}

## 何となぜ？
デバッガを使用するということは、専門的なツールにアクセスして、コードの中身を覗き込み、ステップバイステップで実行を見守ることを意味します。プログラマーは、バグを潰し、パフォーマンスを最適化し、コードの挙動を理解するためにこれを行います。

## 方法：
ここに期待通りに動作していないJavaScriptのコードの一部があります：

```javascript
function buggyMultiply(a, b) {
    return a + b; // おっと！これは加算ではなく乗算であるべきです。
}

let result = buggyMultiply(5, 3);
console.log('Result:', result);
```

出力は間違っています：
```
Result: 8
```

Chrome DevToolsでデバッグしましょう：

1. ブラウザでこのJSを開きます。
2. 右クリックして「検証」を選択し、DevToolsを開きます。
3. 「Sources」タブをクリックします。
4. コードスニペットまたはページを見つけて、`return`ステートメントの行番号をクリックしてブレークポイントを設定します。
5. ブレークポイントをトリガするためにページを更新します。
6. 「Scope」パネルをチェックして、ローカル変数`a`と`b`を確認します。
7. 「次の関数コールをステップオーバー」ボタンでステップ実行します。
8. `return`ステートメントでバグを見つけます。
9. コードを修正します：
```javascript
function buggyMultiply(a, b) {
    return a * b; // 修正済み！
}

let result = buggyMultiply(5, 3);
console.log('Result:', result);
```

修正された出力：
```
Result: 15
```

## 深掘り
デバッグの概念は、コンピューティングの初期から存在しています—伝説によると、1940年代にコンピュータ内で蛾が見つかったときに始まったと言われています！今日では、JavaScriptデバッガー（ブラウザーの内蔵ツール（Chrome DevTools、Firefox Developer Tools）やIDE統合デバッガー（Visual Studio Code、WebStorm）など）はたくさんの機能を提供しています。

内蔵デバッガーの代わりには、WebStormや古くからある`console.log`を使用して変数の状態を出力するなどのサードパーティツールがあります。しかし、これらはデバッガーが提供するリアルタイムの対話や詳細な検査を提供しません。

実装の詳細については、ほとんどのデバッガーが同様に動作します：実行を一時停止するブレークポイントを設定し、コードをステップ実行し、現在の変数状態を検査し、式を監視し、さらには異なるシナリオをテストするために値を動的に変更することさえ可能です。

## 参照
- [Google Chrome DevTools](https://developers.google.com/web/tools/chrome-devtools)
- [Mozilla Developer Network - Firefox Debugger](https://developer.mozilla.org/ja/docs/Tools/Debugger)
- [Visual Studio Code - デバッグ](https://code.visualstudio.com/docs/editor/debugging)
