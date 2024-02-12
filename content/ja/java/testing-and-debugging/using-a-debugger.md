---
title:                "デバッガーの使い方"
aliases:
- /ja/java/using-a-debugger.md
date:                  2024-01-26T03:50:15.106700-07:00
model:                 gpt-4-0125-preview
simple_title:         "デバッガーの使い方"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/using-a-debugger.md"
---

{{< edit_this_page >}}

## 何となぜ？
デバッガーを使用するとは、コードのバグをテストして修正するためのツールを使うことを意味します。プログラマーは、アプリケーションの流れを理解し、エラーの原因を特定し、実行中のロジックを検証するためにこれを行います。

## 方法：
例えば、何かおかしいJavaプログラムがあり、その理由がわからないとします。Eclipseのような人気のあるJava開発用IDEを使ってデバッガーを起動する方法は以下の通りです:

まず、ブレークポイントを設定していることを確認してください。次に、ファイルを右クリックし、「デバッグとして実行」を選択し、そして「Javaアプリケーション」をクリックします。

```Java
public class DebugExample {
    public static void main(String[] args) {
        int a = 5;
        int b = 0;
        // ここにブレークポイントを設定します
        int result = divide(a, b);
        System.out.println("結果は: " + result);
    }

    private static int divide(int numerator, int denominator) {
        // 別のブレークポイントを設定するのに良い場所
        return numerator / denominator;
    }
}
```

これを行うと、プログラムはブレークポイントで一時停止し、変数を検査したり、コードを一行ずつ進めたり、プログラムの動作を観察したりできます。

デバッガーコンソールでのサンプル出力:
```
ブレークポイントがヒットしました: int result = divide(a, b);
```

## 深掘り
デバッグの概念はプログラミングの初期から存在しています。伝説によれば、「バグ」という言葉は、分野の先駆者であるグレース・ホッパーがコンピューター内に実際にいたモスのような虫を見つけたことに由来すると言われています。今日に至るまで、IntelliJ IDEA, Eclipse, NetBeansといった強力なデバッガーを搭載した洗練されたIDEがあります。

IDEデバッガーの代替手段には、ログ記録、print文（貧乏人のデバッガー）、アサーション、Java Development Kit (JDK)の一部であるjdb（Java Debugger）のようなスタンドアローンのデバッグツールが含まれます。

デバッガーは、プログラマーが実行を一時停止（ブレークポイント）、コードを逐次実行、変数の値を検査、その値をその場で変更、さらにはコードブロックをブロックごとに実行することを可能にすることで機能します。デバッガーの使用は、問題を引き起こしている正確なコード行を見つけることが干し針を見つけるようなものである複雑なアプリケーションを開発する際に非常に貴重な技術と考えられています。

## 参照
- デバッグに関する公式Oracleドキュメント: [Oracle Java SE デバッグ](https://docs.oracle.com/javase/8/docs/technotes/tools/windows/jdb.html)
- Eclipseのデバッグガイド: [Eclipse デバッグのヒント](https://www.eclipse.org/community/eclipse_newsletter/2017/june/article4.php)
- 複数のコマンドラインJDKツールと軽量プロファイリング機能を統合した視覚的ツール、VisualVM: [VisualVM](https://visualvm.github.io/)
