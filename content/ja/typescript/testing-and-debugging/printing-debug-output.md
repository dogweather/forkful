---
date: 2024-01-20 17:53:38.716716-07:00
description: "How to: (\u65B9\u6CD5) \u30C7\u30D0\u30C3\u30B0\u51FA\u529B\u306F\u53E4\
  \u304F\u304B\u3089\u958B\u767A\u306E\u91CD\u8981\u306A\u90E8\u5206\u3067\u3059\u3002\
  `console.log`\u306F\u5358\u5143\u30C6\u30B9\u30C8\u3084\u30C7\u30D0\u30C3\u30AC\u30C4\
  \u30FC\u30EB\u306E\u524D\u306B\u4E3B\u8981\u306A\u30C7\u30D0\u30C3\u30B0\u624B\u6BB5\
  \u3067\u3057\u305F\u3002\u4ECA\u65E5\u3067\u3082\u3001\u7C21\u5358\u306B\u4F7F\u3048\
  \u3066\u624B\u8EFD\u306A\u305F\u3081\u3001\u3088\u304F\u4F7F\u308F\u308C\u3066\u3044\
  \u307E\u3059\u3002\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:37:50.058075-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) \u30C7\u30D0\u30C3\u30B0\u51FA\u529B\u306F\u53E4\u304F\u304B\
  \u3089\u958B\u767A\u306E\u91CD\u8981\u306A\u90E8\u5206\u3067\u3059\u3002`console.log`\u306F\
  \u5358\u5143\u30C6\u30B9\u30C8\u3084\u30C7\u30D0\u30C3\u30AC\u30C4\u30FC\u30EB\u306E\
  \u524D\u306B\u4E3B\u8981\u306A\u30C7\u30D0\u30C3\u30B0\u624B\u6BB5\u3067\u3057\u305F\
  \u3002\u4ECA\u65E5\u3067\u3082\u3001\u7C21\u5358\u306B\u4F7F\u3048\u3066\u624B\u8EFD\
  \u306A\u305F\u3081\u3001\u3088\u304F\u4F7F\u308F\u308C\u3066\u3044\u307E\u3059\u3002"
title: "\u30C7\u30D0\u30C3\u30B0\u51FA\u529B\u3092\u8868\u793A\u3059\u308B"
weight: 33
---

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
