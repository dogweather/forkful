---
aliases:
- /ja/typescript/refactoring/
date: 2024-01-26 03:36:51.624724-07:00
description: "\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\u30B0\u3068\u306F\u3001\u65E2\
  \u5B58\u306E\u30B3\u30F3\u30D4\u30E5\u30FC\u30BF\u30B3\u30FC\u30C9\u306E\u69CB\u9020\
  \u3092\u5909\u66F4\u3059\u308B\u30D7\u30ED\u30BB\u30B9\u3067\u3042\u308A\u306A\u304C\
  \u3089\u3001\u305D\u306E\u5916\u90E8\u306E\u632F\u308B\u821E\u3044\u3092\u5909\u3048\
  \u306A\u3044\u3053\u3068\u3092\u6307\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u306F\u3001\u30B3\u30FC\u30C9\u3092\u3088\u308A\u30AF\u30EA\u30FC\u30F3\
  \u306B\u3001\u3088\u308A\u4FDD\u5B88\u53EF\u80FD\u306B\u3057\u3001\u8907\u96D1\u3055\
  \u3092\u6E1B\u3089\u3059\u3053\u3068\u3067\u3001\u65B0\u305F\u306B\u53D6\u308A\u7D44\
  \u3080\u4EBA\u304C\u7406\u89E3\u3057\u3084\u3059\u304F\u3059\u308B\u305F\u3081\u306B\
  \u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
lastmod: 2024-02-18 23:08:54.690077
model: gpt-4-0125-preview
summary: "\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\u30B0\u3068\u306F\u3001\u65E2\
  \u5B58\u306E\u30B3\u30F3\u30D4\u30E5\u30FC\u30BF\u30B3\u30FC\u30C9\u306E\u69CB\u9020\
  \u3092\u5909\u66F4\u3059\u308B\u30D7\u30ED\u30BB\u30B9\u3067\u3042\u308A\u306A\u304C\
  \u3089\u3001\u305D\u306E\u5916\u90E8\u306E\u632F\u308B\u821E\u3044\u3092\u5909\u3048\
  \u306A\u3044\u3053\u3068\u3092\u6307\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u306F\u3001\u30B3\u30FC\u30C9\u3092\u3088\u308A\u30AF\u30EA\u30FC\u30F3\
  \u306B\u3001\u3088\u308A\u4FDD\u5B88\u53EF\u80FD\u306B\u3057\u3001\u8907\u96D1\u3055\
  \u3092\u6E1B\u3089\u3059\u3053\u3068\u3067\u3001\u65B0\u305F\u306B\u53D6\u308A\u7D44\
  \u3080\u4EBA\u304C\u7406\u89E3\u3057\u3084\u3059\u304F\u3059\u308B\u305F\u3081\u306B\
  \u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
title: "\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\u30B0"
---

{{< edit_this_page >}}

## 何となぜ？
リファクタリングとは、既存のコンピュータコードの構造を変更するプロセスでありながら、その外部の振る舞いを変えないことを指します。プログラマーは、コードをよりクリーンに、より保守可能にし、複雑さを減らすことで、新たに取り組む人が理解しやすくするためにこれを行います。

## 方法：
見た目があまり良くないTypeScriptの関数があると考えます - 少々乱雑で、いくらかの愛情と手当を必要としています：

```typescript
function userInfo(data: any): string {
    return "User Info: " + data.name + ", " + data.age + ", " + data.email + ";" ;
}
```
リファクタリングされたこの例は、以下のようになるかもしれません：

```typescript
interface User {
    name: string;
    age: number;
    email: string;
}

function formatUserInfo(user: User): string {
    return `User Info: ${user.name}, ${user.age}, ${user.email};`;
}
```

二つ目の例はより堅牢で、TypeScriptの型システムを`interface`で利用することで、実行時エラーの可能性を避け、可読性を向上させています。

## 深堀り
リファクタリングは現代の概念ではありません。プログラミングと共に進化し、1999年にマーティン・ファウラーの本「リファクタリング: 既存のコードの設計を改善する」のリリースとともに、より形式化されました。アジャイル開発環境では不可欠で、適応的なコード変更を容易にします。手動でのリファクタリングに代わるものとして、TSLintやTypeScript自身の言語サーバーのような自動化ツールがあり、特定のリファクタリング作業を提案したりさえも実行してくれることがあります。実装の詳細は通常、「コードの臭い」、例えば重複するコード、長いメソッド、または大きなクラスなどを認識し、メソッドの抽出、より適切なクラスへの移動、またはより単純な構造の使用などのパターンを適用して対処することに関わります。これらのパターンは、リファクタリングの方法と理由を理解するための鍵となります。

## 参照も見てください
- [マーティン・ファウラーの本「リファクタリング: 既存のコードの設計を改善する」](https://martinfowler.com/books/refactoring.html)
- [静的コード解析のためのTSLint](https://palantir.github.io/tslint/)
- [コードの臭いを理解する](https://refactoring.guru/refactoring/smells)
