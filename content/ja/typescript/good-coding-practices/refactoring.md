---
title:                "リファクタリング"
aliases: - /ja/typescript/refactoring.md
date:                  2024-01-26T03:36:51.624724-07:00
model:                 gpt-4-0125-preview
simple_title:         "リファクタリング"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/refactoring.md"
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
