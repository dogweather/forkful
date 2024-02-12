---
title:                "デバッグ出力を表示する"
aliases:
- /ja/typescript/printing-debug-output/
date:                  2024-01-20T17:53:38.716716-07:00
model:                 gpt-4-1106-preview
simple_title:         "デバッグ出力を表示する"

tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
デバッグ出力はバグの原因を見つけるためにプログラムの実行中の変数や状態を表示します。コードの動きを理解し、問題を解決するのに役立ちます。

## How to: (方法)
```TypeScript
// Basic console.log usage
let message: string = "Hello, Debugging World!";
console.log(message);  // Output: Hello, Debugging World!

// Printing an object
let user: { name: string, age: number } = { name: "Yuko", age: 28 };
console.log(user);  // Output: { name: 'Yuko', age: 28 }

// Using string interpolation
let productName: string = "ポテトチップス";
let price: number = 150;
console.log(`${productName}の値段は${price}円です。`); // Output: ポテトチップスの値段は150円です。

// Debugging with console.error and console.warn
let errorMessage: string = "エラーが発生しました！";
console.error(errorMessage); // Outputs error message to the console

let warningMessage: string = "注意が必要です。";
console.warn(warningMessage); // Outputs warning message to the console
```

## Deep Dive (深掘り)
デバッグ出力は古くから開発の重要な部分です。`console.log`は単元テストやデバッガツールの前に主要なデバッグ手段でした。今日でも、簡単に使えて手軽なため、よく使われています。

選択肢として、より高度なデバッグ技術もあります。たとえば、統合開発環境(IDE)のデバッグ機能、JavaScriptの`debugger`ステートメント、あるいはソースマップを使ったブラウザの開発者ツールです。

TypeScriptでは、コンパイルオプションの設定やソースマップの生成に注意する必要があります。そうすることで、TypeScriptのコードと出力されたJavaScriptコードの間でのデバッグがスムーズに行えます。

## See Also (参照)
- [MDN Web Docs Console API](https://developer.mozilla.org/en-US/docs/Web/API/console)
- [TypeScript Handbook - Compiler Options](https://www.typescriptlang.org/docs/handbook/compiler-options.html)
- [Node.js Documentation - Console](https://nodejs.org/api/console.html)
