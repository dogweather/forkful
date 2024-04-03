---
date: 2024-01-26 00:53:59.828141-07:00
description: "\u3069\u306E\u3088\u3046\u306B\u884C\u3046\u304B\uFF1A Java\u306F\u4F8B\
  \u5916\u3092\u4F7F\u7528\u3057\u3066\u30A8\u30E9\u30FC\u3092\u51E6\u7406\u3057\u307E\
  \u3059\u3002\u30EA\u30B9\u30AF\u306E\u3042\u308B\u30B3\u30FC\u30C9\u3092`try`\u30D6\
  \u30ED\u30C3\u30AF\u3067\u56F2\u307F\u3001\u4F8B\u5916\u306F`catch`\u3067\u6355\u6349\
  \u3057\u307E\u3059\u3002\u3053\u3053\u306B\u7C21\u5358\u306A\u4F8B\u3092\u793A\u3057\
  \u307E\u3059\uFF1A."
lastmod: '2024-03-13T22:44:41.958946-06:00'
model: gpt-4-1106-preview
summary: "Java\u306F\u4F8B\u5916\u3092\u4F7F\u7528\u3057\u3066\u30A8\u30E9\u30FC\u3092\
  \u51E6\u7406\u3057\u307E\u3059\u3002\u30EA\u30B9\u30AF\u306E\u3042\u308B\u30B3\u30FC\
  \u30C9\u3092`try`\u30D6\u30ED\u30C3\u30AF\u3067\u56F2\u307F\u3001\u4F8B\u5916\u306F\
  `catch`\u3067\u6355\u6349\u3057\u307E\u3059\u3002\u3053\u3053\u306B\u7C21\u5358\u306A\
  \u4F8B\u3092\u793A\u3057\u307E\u3059\uFF1A."
title: "\u30A8\u30E9\u30FC\u51E6\u7406"
weight: 16
---

## どのように行うか：
Javaは例外を使用してエラーを処理します。リスクのあるコードを`try`ブロックで囲み、例外は`catch`で捕捉します。ここに簡単な例を示します：

```java
public class ErrorHandlingExample {
    public static void main(String[] args) {
        try {
            int result = divide(10, 0);
            System.out.println("結果は: " + result);
        } catch (ArithmeticException e) {
            System.out.println("おっと、ゼロで割ることはできません！");
        }
    }

    private static int divide(int numerator, int denominator) {
        return numerator / denominator;
    }
}
```

出力：
```
おっと、ゼロで割ることはできません！
```

## 詳細
Javaのエラー処理は進化しています。初期には例外はなく、プログラマーはエラーコードをチェックしていました。その後、Javaはtry-catchブロックを導入し、より洗練されたエラー処理が可能になりました。

従来の`try-catch`に代わるものとして、Java 7で導入された、リソースの自動クロージングとよりきれいなコードのための`try-with-resources`があります。

実装の詳細は重要です。例えば、`Exception`や`Throwable`を捕捉することは通常、悪い習慣です。それはあまりにも幅広く、あなたが気づいていないかもしれないバグを隠蔽します。特定の例外に留めておきましょう。

## 参照
- Oracleの公式Javaチュートリアルの例外について: [https://docs.oracle.com/javase/tutorial/essential/exceptions/](https://docs.oracle.com/javase/tutorial/essential/exceptions/)
- Javaの`try-with-resources`ステートメントドキュメント: [https://docs.oracle.com/javase/tutorial/essential/exceptions/tryResourceClose.html](https://docs.oracle.com/javase/tutorial/essential/exceptions/tryResourceClose.html)
- Joshua Blochの『Effective Java』：例外に関するベストプラクティス
