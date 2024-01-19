---
title:                "デバッグ出力の印刷"
html_title:           "Fish Shell: デバッグ出力の印刷"
simple_title:         "デバッグ出力の印刷"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/printing-debug-output.md"
---

{{< edit_this_page >}}

# Javaでデバッグ出力を理解しよう

## 何となぜ？ (What & Why?)
デバッグ出力はコードの動きを追跡・確認する手法です。これにより、プログラマーは潜在的な部分的なエラーや誤割り当てを発見できます。

## 方法 (How to)
Java では System.out.println を使ってデバッグ出力を行います。 以下に例を示しました：

``` Java
public class DebugExample {
    public static void main(String[] args) {
        int a = 5;
        int b = 10;
        System.out.println("Debug: a + b = " + (a + b));
    }
}
```
このプログラムを実行すると、次の出力が得られます：

```
Debug: a + b = 15
```

ばっちりです、デバッグ出力がうまく機能しています！

## ディープダイブ (Deep Dive) 
### 歴史的な文脈
デバッグ出力はコンピューティングが始まったときすぐに生まれたテクニックで、プログラムが正しく動かない原因を調べるために使われてきました。

### 代替手段
多くの統合開発環境(IDE)はより高度なデバッグツールを提供しているため、デバッグ出力は基本的なエラー調査に使われます。

### 実装詳細
System.out.println は標準出力ストリームに接続され、出力は通常コンソールに表示されます。

## 参考資料 (See Also)
- Oracleのチュートリアル: [デバッグ](https://docs.oracle.com/javase/tutorial/getStarted/debug/)
- StackOverflow: [Javaでのデバッグ方法](https://stackoverflow.com/questions/2535678/how-to-debug-a-java-program)
- Baeldung: [Javaでのログの使用](https://www.baeldung.com/java-logging-intro)