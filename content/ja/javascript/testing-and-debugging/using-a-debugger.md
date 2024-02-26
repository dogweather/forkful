---
date: 2024-01-26 03:50:16.985448-07:00
description: "\u30C7\u30D0\u30C3\u30AC\u3092\u4F7F\u7528\u3059\u308B\u3068\u3044\u3046\
  \u3053\u3068\u306F\u3001\u5C02\u9580\u7684\u306A\u30C4\u30FC\u30EB\u306B\u30A2\u30AF\
  \u30BB\u30B9\u3057\u3066\u3001\u30B3\u30FC\u30C9\u306E\u4E2D\u8EAB\u3092\u8997\u304D\
  \u8FBC\u307F\u3001\u30B9\u30C6\u30C3\u30D7\u30D0\u30A4\u30B9\u30C6\u30C3\u30D7\u3067\
  \u5B9F\u884C\u3092\u898B\u5B88\u308B\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\
  \u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30D0\u30B0\u3092\u6F70\u3057\
  \u3001\u30D1\u30D5\u30A9\u30FC\u30DE\u30F3\u30B9\u3092\u6700\u9069\u5316\u3057\u3001\
  \u30B3\u30FC\u30C9\u306E\u6319\u52D5\u3092\u7406\u89E3\u3059\u308B\u305F\u3081\u306B\
  \u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
lastmod: '2024-02-25T18:49:40.628491-07:00'
model: gpt-4-0125-preview
summary: "\u30C7\u30D0\u30C3\u30AC\u3092\u4F7F\u7528\u3059\u308B\u3068\u3044\u3046\
  \u3053\u3068\u306F\u3001\u5C02\u9580\u7684\u306A\u30C4\u30FC\u30EB\u306B\u30A2\u30AF\
  \u30BB\u30B9\u3057\u3066\u3001\u30B3\u30FC\u30C9\u306E\u4E2D\u8EAB\u3092\u8997\u304D\
  \u8FBC\u307F\u3001\u30B9\u30C6\u30C3\u30D7\u30D0\u30A4\u30B9\u30C6\u30C3\u30D7\u3067\
  \u5B9F\u884C\u3092\u898B\u5B88\u308B\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\
  \u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30D0\u30B0\u3092\u6F70\u3057\
  \u3001\u30D1\u30D5\u30A9\u30FC\u30DE\u30F3\u30B9\u3092\u6700\u9069\u5316\u3057\u3001\
  \u30B3\u30FC\u30C9\u306E\u6319\u52D5\u3092\u7406\u89E3\u3059\u308B\u305F\u3081\u306B\
  \u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
title: "\u30C7\u30D0\u30C3\u30AC\u30FC\u306E\u4F7F\u3044\u65B9"
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
