---
title:                "エラー処理"
aliases:
- /ja/java/handling-errors/
date:                  2024-01-26T00:53:59.828141-07:00
model:                 gpt-4-1106-preview
simple_title:         "エラー処理"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/handling-errors.md"
---

{{< edit_this_page >}}

## 何となぜ？

エラー処理は、うまくいかないことを予測して、それに対処するコードを書くことを意味します。プログラマーは、ソフトウェアを堅牢にし、クラッシュや奇妙な動作を防ぐためにこれを行います。

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
